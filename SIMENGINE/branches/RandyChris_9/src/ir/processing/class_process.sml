signature CLASSPROCESS =
sig

    (* Optimization method - primary method used to call optimizations that get executed across a class. *)
    (* Note: All class optimizations work on expressions that are references, therefore most methods don't have a return value *)
    val optimizeClass : DOF.class -> unit

    (* Accessor methods *)    
    val class2instnames : DOF.class -> (Symbol.symbol * Symbol.symbol) list (* return a list of instances within the class as classname/instname tuples *)
    val class2orig_name : DOF.class -> Symbol.symbol (* the name of the class before it was renamed, for example during ordering *)
    val class2classname : DOF.class -> Symbol.symbol (* returns the class name as stored in the structure, not the "realclassname" *)
    val findSymbols : DOF.class -> Symbol.symbol list (* return a list of every unique symbol name used in the class - helpful for global renaming *)
    val class2states : DOF.class -> Symbol.symbol list (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesbyiterator : Symbol.symbol -> DOF.class -> Symbol.symbol list (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesize : DOF.class -> int (* returns the total number of states stored in the class *)
    val class2statesizebyiterator : Symbol.symbol -> DOF.class -> int (* limit the state count to those associated with a particular temporal iterator *)
    val symbol2exps : DOF.class -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val class2update_states : DOF.class -> Symbol.symbol list (* find all state names that have an update equation associated *)

    (* Functions to modify class properties, usually recursively through the expressions *)
    val duplicateClass : DOF.class -> Symbol.symbol -> DOF.class (* makes a duplicate class with the new supplied name *)
    val pruneClass : DOF.systemiterator option -> DOF.class -> unit (* prunes unneeded equations in the class, the initial bool causes all states to be kept as well *)
    val propagateIterators : DOF.class -> unit (* propagates iterators through equations into outputs *)
    val assignCorrectScope : DOF.class -> unit (* sets read state or write state properties on symbols *)
    val addEPIndexToClass : bool -> DOF.class -> unit (* sets the embarrassingly parallel property on symbols in all but the top level class *)
    val makeSlaveClassProperties : DOF.classproperties -> DOF.classproperties (* updates a class to make it a slave class - this is one that shouldn't write any states but can generate intermediates *)
    val fixSymbolNames : DOF.class -> unit (* makes all symbol names C-compliant *)
    val sym2codegensym : Symbol.symbol -> Symbol.symbol (* helper function used by fixSymbolNames to update just one symbol *)
    val renameInsts : (Symbol.symbol * Symbol.symbol) -> DOF.class -> unit (* change all instance names in a class *)
    val createEventIterators : DOF.class -> unit (* searches out postprocess and update iterators *)

end
structure ClassProcess : CLASSPROCESS = 
struct

val i2s = Util.i2s
val e2s = ExpPrinter.exp2str

fun duplicateClass (class: DOF.class) new_name =
    let
	val {name, properties, inputs, outputs, exps, iterators} = class						       
    in
	{name=new_name,
	 iterators=iterators,
	 properties=properties,
	 inputs=ref (!inputs),
	 outputs=ref (!outputs),
	 exps=ref (!exps)}
    end

fun renameInsts (orig_name, new_name) (class: DOF.class) =
    let	
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val rename = ExpProcess.renameInst (orig_name, new_name)
	val exps' = map rename exps
	val outputs' = map
			   (fn{name,condition,contents}=>{name=name,
							  condition=rename condition,
							  contents=map rename contents})
			   outputs
    in
	(#exps class := exps';
	 #outputs class := outputs')
    end

fun getStates (class:DOF.class) =
    let
	val exps = !(#exps class)
    in
	map (Term.sym2curname o ExpProcess.exp2term o ExpProcess.lhs) (List.filter ExpProcess.isStateEq exps)
    end

fun isSymInput (class:DOF.class) sym = 
    let
	val inputs = !(#inputs class)
    in
	List.exists (fn{name,...}=>Term.sym2curname name = sym) inputs
    end

fun isSymOutput (class:DOF.class) sym = 
    let
	val outputs = !(#outputs class)
    in
	List.exists (fn{name,...}=>Term.sym2curname name = sym) outputs
    end

fun findMatchingEq (class:DOF.class) sym =  
    let
	val exps = !(#exps class)
	(* we generally want to avoid initial conditions since there are better equations to look at ... *)
	val (init_equs, exps) = List.partition ExpProcess.isInitialConditionEq exps
	val outputs = !(#outputs class)
    in
	case (List.find (fn(exp)=> 
			   List.exists (fn(sym')=>sym=sym') (ExpProcess.exp2symbols (ExpProcess.lhs exp))) exps) of
	    SOME exp => SOME exp
	  | NONE => (* check outputs *)
	    (case (List.find (fn{name,...}=> Term.sym2curname name = sym) outputs) of
		 SOME {name, contents, condition} => (case contents of
							  [] => DynException.stdException(("No equations define output '"^(e2s (Exp.TERM name))^"'"),
											  "ClassProcess.findMatchingEq",
											  Logger.INTERNAL)
							| [oneexp] => SOME (ExpBuild.equals (Exp.TERM name, oneexp))
							| rest => SOME (ExpBuild.equals (Exp.TERM name, ExpBuild.group rest)))
	       | NONE => NONE)
	     
    end

(* flattenEq does not pass through instance equations - we need a different one that will pass through instance equations *)
fun flattenEq (class:DOF.class) sym = 
    ((*Util.log ("Calling flattenEq on sym '"^(Symbol.name sym)^"'");*)
    if isSymInput class sym then
	Exp.TERM (#name (valOf (List.find (fn{name,...}=>Term.sym2curname name = sym) (!(#inputs class)))))
    else
	case findMatchingEq class sym of
	    SOME exp => 
	    let
		(*val _ = Util.log ("Found matching eq for sym '"^(Symbol.name sym)^"' -> '"^(e2s exp)^"'")*)
		val symbols = ExpProcess.exp2termsymbols (ExpProcess.rhs exp)
		val local_symbols = List.filter Term.isLocal symbols
		val matching_equations = map ((findMatchingEq class) o Term.sym2curname) local_symbols
		(* only use symbols that are from intermediates *)
		val filtered_symbols = map (fn(sym, equ)=>sym) 
					   (List.filter 
						(fn(_,equ)=> case equ of 
								 SOME e => not (ExpProcess.isInstanceEq e)
							       | NONE => false)
						(ListPair.zip (local_symbols,matching_equations)))
		val local_symbols_rewrite_rules = 
		    map
			(fn(termsym)=>
			   let
			       val find = Exp.TERM termsym
			       val test = NONE
			       val repl_exp = ExpProcess.rhs (flattenEq class (Term.sym2curname termsym))
			       (*val _ = Util.log ("flattenEq ("^(Symbol.name (#name class))^"): '"^(e2s find)^"' ->  '"^(e2s repl_exp)^"'")*)
			       val replace = Rewrite.RULE repl_exp
			   in
			       {find=find,
				test=test,
				replace=replace}			       
			   end)
			filtered_symbols
	    in
		Match.applyRewritesExp local_symbols_rewrite_rules exp
	    end
	  | NONE => DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenEq", Logger.INTERNAL))



(*this will take an exp, like 'a+b', and search for a, and search for b, substituting both variables back into the expression *)
fun flattenExp (class:DOF.class) exp =
    let
	val symbols = ExpProcess.exp2symbols exp
	val equations = map (flattenEq class) symbols
	val rules = map ExpProcess.equation2rewrite equations
    in
	Match.applyRewritesExp rules exp
    end
    handle e => DynException.checkpoint "ClassProcess.flattenExp" e

fun findSymbols (class: DOF.class) =
    let
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)
	val exps = !(#exps class)

	fun input2symbols (inp as {name, default}) =
	    ExpProcess.exp2symbols (Exp.TERM (name)) @ 
	    (case default of
		 SOME v => ExpProcess.exp2symbols v
	       | NONE => [])

	fun output2symbols (out as {name, contents, condition}) =
	    (ExpProcess.exp2symbols (Exp.TERM name)) @
	    (Util.flatmap ExpProcess.exp2symbols contents) @
	    (ExpProcess.exp2symbols condition)

	fun exp2symbols exp =
	    ExpProcess.exp2symbols exp
	    
    in
	Util.uniquify (Util.uniquify (Util.flatmap input2symbols inputs) @
		       Util.uniquify ((Util.flatmap output2symbols outputs)) @
		       Util.uniquify ((Util.flatmap exp2symbols exps)))
    end

fun findStateSymbols (class: DOF.class) =
    let
	val exps = !(#exps class)

	fun exp2symbols exp =
	    let
		val terms = ExpProcess.exp2termsymbols exp
		val state_terms = List.filter 
				      (fn(t)=>Term.isReadState t orelse
					      Term.isWriteState t)
				      terms
		val state_exps = map Exp.TERM state_terms
	    in
		Util.flatmap ExpProcess.exp2symbols state_exps
	    end
	    
    in
	Util.uniquify ((Util.flatmap exp2symbols exps))
    end

fun renameSym (orig_sym, new_sym) (class: DOF.class) =
    let
	(*val eqs = !(#eqs class)*)
	val exps = !(#exps class)
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)		      

	val exp_rename = ExpProcess.renameSym (orig_sym, new_sym)

	fun renameInput (input as {name, default}) =
	    {name=ExpProcess.exp2term (exp_rename (Exp.TERM name)),
	     default=case default of SOME v => SOME (exp_rename v) | NONE => NONE}
		      
	fun renameOutput (output as {name, contents, condition}) = 
	    {name=ExpProcess.exp2term (exp_rename (Exp.TERM name)),
	     contents=map exp_rename contents,
	     condition=exp_rename condition}
    in
	((*(#eqs class) := (map (EqUtil.renameSym (orig_sym, new_sym)) eqs);*)
	 (#exps class) := (map (ExpProcess.renameSym (orig_sym, new_sym)) exps);
	 (#inputs class) := (map renameInput inputs);
	 (#outputs class) := (map renameOutput outputs))
    end

val commonPrefix = "mdlvar__"
val parallelSuffix = "[modelid]"

fun removePrefix str = 
    if String.isPrefix commonPrefix str then
	String.extract (str, String.size commonPrefix, NONE)
    else
	str

fun fixname name = 
    let
	fun lbrack c = if c = "[" then "" else c
	fun rbrack c = if c = "]" then "" else c
	fun period c = if c = "." then "__" else c
	fun dash c = if c = "-" then "_" else c
	fun space c = if c = " " then "_" else c
	fun underscore c = if c = "_" then "_" else c
	fun lparen c = if c = "(" then "" else c
	fun rparen c = if c = ")" then "" else c
	fun plus c = if c = "+" then "" else c					       
    in
	(StdFun.stringmap (lbrack o rbrack o period o dash o space o underscore o lparen o rparen o plus) name)
    end

fun sym2codegensym sym =
    Symbol.symbol (commonPrefix ^ (fixname (Symbol.name sym)))

(* fix according to C rules *)
fun fixSymbolNames (class: DOF.class) =
    let
	val symbols = findSymbols class
	val iterators = CurrentModel.iterators()
	fun fixsym sym = if List.exists (fn(sym',_)=>sym=sym') iterators then
			     sym (* no need to fix 't' or another iterator - it's reserved *)
			 else
			     sym2codegensym sym
    in
	app (fn(sym)=>
	       let
		   val sym' = fixsym sym
	       in
		   if sym = sym' then
		       ()
		   else
		       renameSym (sym, sym') class
	       end) 
	    symbols
    end

fun class2orig_name (class : DOF.class) =
    let
	val {properties={classtype,...},...} = class
    in
	case classtype of
	    DOF.MASTER c => c
	  | DOF.SLAVE c => c
    end

fun class2classname (class : DOF.class) =
    let
	val {name, ...} = class
    in
	name
    end

fun class2states (class : DOF.class) =
    let
	val exps = !(#exps class)
	val init_cond_states = map ExpProcess.getLHSSymbol (List.filter ExpProcess.isInitialConditionEq exps)
	val dynamic_states = map ExpProcess.getLHSSymbol (List.filter ExpProcess.isStateEq exps)
    in
	Util.uniquify (init_cond_states @ dynamic_states)
    end

fun class2statesbyiterator iter_sym (class : DOF.class) =
    let
	val exps = !(#exps class)
	val init_cond_states = map ExpProcess.getLHSSymbol (List.filter (ExpProcess.doesEqHaveIterator iter_sym) (List.filter ExpProcess.isInitialConditionEq exps))
	val dynamic_states = map ExpProcess.getLHSSymbol (List.filter (ExpProcess.doesEqHaveIterator iter_sym) (List.filter ExpProcess.isStateEq exps))
    in
	Util.uniquify (init_cond_states @ dynamic_states)
    end

(* match all the expressions that have that symbol on the lhs *)
fun symbol2exps (class: DOF.class) sym =
    List.filter (fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) (!(#exps class))

(* return those states that update the value of a state that already has a dynamic equation *)
fun class2update_states (class : DOF.class) =
    let
	val states = class2states class

	fun hasInitEq exps =
	    List.exists ExpProcess.isInitialConditionEq exps

	fun hasStateEq exps =
	    List.exists ExpProcess.isStateEq exps

	fun hasUpdateEq exps =
	    List.exists ExpProcess.isIntermediateEq exps

	(* let's assume that they have an init eq, a state eq, and possibly an update eq *)
	(* really, it's ok if it just has an init eq and an update eq - that means it's an event state *)
	val states_with_updates = 
	    List.filter
		(fn(sym)=>
		   let val exps' = symbol2exps class sym
		   in (hasInitEq exps') andalso (hasStateEq exps') andalso (hasUpdateEq exps')
		   end)
		states
    in
	states_with_updates
    end

fun class2postprocess_states (class: DOF.class) = 
    let
	val states = class2states class

	fun hasInitEq exps =
	    List.exists ExpProcess.isInitialConditionEq exps

	fun hasStateEq exps =
	    List.exists ExpProcess.isStateEq exps

	fun hasUpdateEq exps =
	    List.exists ExpProcess.isIntermediateEq exps

	(* the only difference between a post process state and an update state is that the post process state does not have a state equation *)
	val post_process_states = 
	    List.filter
		(fn(sym)=>
		   let val exps' = symbol2exps class sym
		   in (hasInitEq exps') andalso (not (hasStateEq exps')) andalso (hasUpdateEq exps')
		   end)
		states
    in
	post_process_states
    end

fun createEventIterators (class: DOF.class) =
    let
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	fun update_exp exp = 
	    case exp of 
		Exp.TERM (Exp.SYMBOL (sym, props)) => 
		(case Property.getIterator props of
		    SOME ((itersym,iterindex)::rest) => Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props ((Symbol.symbol ("update[" ^(Symbol.name itersym) ^ "]"),(*iterindex*)Iterator.RELATIVE 1)::rest)))
		  | _ => exp)
	      | _ => exp

	fun pp_exp exp = 
	    case exp of 
		Exp.TERM (Exp.SYMBOL (sym, props)) => 
		(case Property.getIterator props of
		     SOME ((itersym,iterindex)::rest) => Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props ((Symbol.symbol ("pp[" ^(Symbol.name itersym) ^ "]"),(*iterindex*)Iterator.RELATIVE 1)::rest)))
		   | _ => exp)
	      | _ => exp
			  
	val update_states = class2update_states class
	val postprocess_states = class2postprocess_states class

	val exps' = map
			(fn(exp)=>if ExpProcess.isIntermediateEq exp then
				      let
					  val lhs_sym = ExpProcess.getLHSSymbol exp
				      in
					  if List.exists (fn(sym)=>sym=lhs_sym) update_states then
					      case exp of 
						  Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, rhs]) => 
						  Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [update_exp lhs, rhs])
						| _ => exp
					  else if List.exists (fn(sym)=>sym=lhs_sym) postprocess_states then
					      case exp of 
						  Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [lhs, rhs]) => 
						  Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [pp_exp lhs, rhs])
						| _ => exp
					  else
					      exp
				      end
				  else
				      exp)
			exps
    in
	(#exps class := exps')
    end

fun addEPIndexToClass is_top (class: DOF.class) =
    let
	val master_class = CurrentModel.classname2class (class2orig_name class)
	val states = findStateSymbols master_class
	val exps = !(#exps class)
	val exps' = map (ExpProcess.enableEPIndex is_top states) exps

	val outputs = !(#outputs class)
	val outputs' = map (fn({name, contents, condition})=>
			      {name=name,
			       contents=map (fn(exp)=>ExpProcess.enableEPIndex is_top states exp) contents,
			       condition=ExpProcess.enableEPIndex is_top states condition}
			   ) outputs
    in
	((#exps class) := exps';
	 (#outputs class) := outputs')
    end

fun class2instances class = 
    let
	val exps = !(#exps class)
    in
	List.filter ExpProcess.isInstanceEq exps
    end

fun class2instnames (class : DOF.class) : (Symbol.symbol * Symbol.symbol) list =
    let
	val inst_eqs = class2instances class
	fun inst2orig_names inst =
	    (ExpProcess.instOrigClassName inst, ExpProcess.instOrigInstName inst)

	fun uniq_fun ((c1,i1),(c2,i2)) = i1 = i2
	val classes_insts = Util.uniquify_by_fun uniq_fun (map inst2orig_names inst_eqs)
	val all_classes = CurrentModel.classes()
	fun name2orig_name orig_name = 
	    case List.find (fn{name,...}=>name=orig_name) all_classes of
		SOME {properties={classtype,...},...} => (case classtype of 
							     DOF.MASTER c => c 
							   | DOF.SLAVE c => c)
	      | _ => orig_name
    in
	map (fn(c,i)=>(name2orig_name c, i)) classes_insts
    end

fun class2statesize (class: DOF.class) =
    let
	val {exps,iterators,...} = class
	val initial_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
	val instance_equations = List.filter ExpProcess.isInstanceEq (!exps)
    in
	Util.sum ((map (ExpProcess.exp2size iterators) initial_conditions) @ 
		  (map (fn(exp)=> 
			  let
			      val {classname,...} = ExpProcess.deconstructInst exp
			  in
			      class2statesize (CurrentModel.classname2class classname)
			  end
		       ) instance_equations))
    end

fun class2statesizebyiterator (iter: Symbol.symbol) (class: DOF.class) =
    let	
	val {name,exps,iterators,...} = class
	val initial_conditions = List.filter (ExpProcess.doesEqHaveIterator iter) (List.filter ExpProcess.isInitialConditionEq (!exps))
	val instance_equations = List.filter ExpProcess.isInstanceEq (!exps)

	(*val _ = Util.log ("in class2statesizebyiterator for class '"^(Symbol.name name)^"', # of init conditions="^(i2s (List.length initial_conditions))^", # of instances=" ^ (i2s (List.length instance_equations)))*)
    in
	Util.sum ((map (ExpProcess.exp2size iterators) initial_conditions) @ 
		  (map (fn(exp)=> 
			  let
			      val {classname,...} = ExpProcess.deconstructInst exp
			  in
			      class2statesizebyiterator iter (CurrentModel.classname2class classname)
			  end
		       ) instance_equations))
    end
	

fun makeSlaveClassProperties props = 
    let
	val {classtype, classform, sourcepos} = props
    in
	{classtype=case classtype of
		       DOF.MASTER classname => DOF.SLAVE classname
		     | _ => classtype,
	 classform=classform,
	 sourcepos=sourcepos}
    end


(* this will right now just propagate an iterator from an output to an input *)
fun propagateIterators (class: DOF.class) =
    let
	val assigned_symbols = (map (fn{name,...}=>Exp.TERM name) (!(#inputs class))) @
			       (Util.flatmap (fn(exp)=>
						if ExpProcess.isInitialConditionEq exp then
						    []
						else
						    case (ExpProcess.lhs exp) of
							Exp.TERM (Exp.SYMBOL _) => [ExpProcess.lhs exp]
						      | Exp.TERM (Exp.TUPLE termlist) => map Exp.TERM termlist
						      | _ => DynException.stdException(("Unexpected lhs of equation '"^(e2s exp)^"'"),
										       "ClassProcess.propagateIterators",
										       Logger.INTERNAL)) (!(#exps class)))

	fun create_rewrite_action expsym =
	    let
		
		val spatial_iterators = TermProcess.symbol2spatialiterators (ExpProcess.exp2term expsym)

		val noSpatialIteratorsPredicate = (("is:"^(e2s expsym)), 
						fn(sym)=> 
						  case sym of 
						      Exp.TERM (s as (Exp.SYMBOL (sym', props))) => 
						      sym' = (Term.sym2curname (ExpProcess.exp2term expsym)) andalso
						      (List.length (TermProcess.symbol2spatialiterators s) = 0)
						    | _ => false)

		val pattern = Match.anysym_with_predlist [noSpatialIteratorsPredicate] (Symbol.symbol "a")

		val action = (fn(exp)=>
				case exp of
				    Exp.TERM (s as (Exp.SYMBOL (sym, props))) =>
				    let
					val temporal_iterator = case TermProcess.symbol2temporaliterator s of
								    SOME v => [v]
								  | NONE => []
									      
					val iterators = temporal_iterator @ spatial_iterators
				    in
					Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props iterators))
				    end
				  | _ => exp)

		val rewrite = {find=pattern,
			       test=NONE,
			       replace=Rewrite.ACTION (Symbol.symbol ("AddIterTo:"^(e2s expsym)), action)}

	    in
		rewrite
	    end

	val rewrites = map create_rewrite_action assigned_symbols
	val exps' = map (Match.applyRewritesExp rewrites) (!(#exps class))
	val outputs' = map (fn{name, contents, condition}=>
			     {name=ExpProcess.exp2term (Match.applyRewritesExp rewrites (Exp.TERM name)),
			      condition=Match.applyRewritesExp rewrites condition,
			      contents=map (Match.applyRewritesExp rewrites) contents}) (!(#outputs class))
    in
	(#exps class := exps';
	 #outputs class := outputs')
    end

fun assignCorrectScope (class: DOF.class) =
    let
	val exps = !(#exps class)

	val state_equations = List.filter ExpProcess.isStateEq exps
	val state_terms = map ExpProcess.lhs state_equations
	val state_iterators_options = map (TermProcess.symbol2temporaliterator o ExpProcess.exp2term) state_terms
	val state_iterators = map (fn(exp, iter)=>
				     case iter of
					 SOME i => i
				       | NONE => DynException.stdException(("State '"^(e2s exp)^"' does not have temporal iterator associated with it"), "ClassProcess.assignCorrectScope", Logger.INTERNAL))
				  (ListPair.zip (state_terms, state_iterators_options))

	val symbols = map (Term.sym2curname o ExpProcess.exp2term) state_terms
	(*Util.flatmap ExpProcess.exp2symbols state_terms*)
	val actions = map (fn(sym, iter)=>{find=Match.asym sym, test=NONE, replace=Rewrite.ACTION (sym, (fn(exp)=>ExpProcess.assignCorrectScopeOnSymbol (ExpProcess.prependIteratorToSymbol iter exp)))}) (ListPair.zip (symbols, state_iterators))

	val exps' = map (fn(exp) => Match.applyRewritesExp actions exp) exps

	val outputs = !(#outputs class)
	val outputs' =
	    let
		fun update_output (output as {name, contents, condition}) =
		    let
			val name' = ExpProcess.exp2term (Match.applyRewritesExp actions (ExpProcess.term2exp name))
			val contents' = map (fn(exp) => Match.applyRewritesExp actions exp) contents
			val condition' = Match.applyRewritesExp actions condition
		    in
			{name=name', contents=contents', condition=condition'}
		    end
	    in
		map update_output outputs
	    end

	(*val exps' = map (fn(exp)=>ExpProcess.assignCorrectScope symbols exp) exps*)

	(* write back expression changes *)
	val _ = (#exps class) := exps'
	val _ = (#outputs class) := outputs'

	val outputs = !(#outputs class)
	val outputs' = map (fn{name, contents, condition}=>
			      let
				  (*val _ = Util.log ("Processing output '"^(e2s (Exp.TERM name))^"'")*)
				  (* this line will add the appropriate scope to each symbol and will add the correct temporal iterator*)
				  val contents' = map (Match.applyRewritesExp actions) contents
				  val condition' = Match.applyRewritesExp actions condition

			      (* TODO: The iterator for the name should be defined in modeltranslate.  If it exists,
			       the iterator vector will automatically be added to the output trace.  If it doesn't exist, 
			       only the values will be output.  This can be controlled with the "withtime" and "notime" 
			       properties *)
				  val name' = 
				      case TermProcess.symbol2temporaliterator name of
					  SOME iter => name (* keep the same *)
					| NONE => (* we have to find the iterator *)
					  let
					      (*val _ = Util.log ("Searching for iterator for output '"^(e2s (Exp.TERM name))^"'")*)
					      val sym = Term.sym2curname name
					      val flat_equ = flattenEq class sym
					      (*val _ = Util.log ("Resulting flat equation: " ^ (e2s flat_equ)) *)
					      val symbols = ExpProcess.exp2termsymbols flat_equ
					      val temporal_iterators = 
						  Util.uniquify_by_fun (fn((a,_),(a',_)) => a = a')
								       (List.mapPartial 
									    TermProcess.symbol2temporaliterator 
									    symbols)
					      val spatial_iterators = 
						  Util.uniquify_by_fun (fn((a,_),(a',_)) => a = a')
								       (Util.flatmap 
									    TermProcess.symbol2spatialiterators 
									    symbols)

					      (* assign the temporal iterator first *)
					      val name' = 
						  case temporal_iterators of
						      [] => (* no iterators present, just return name *)			
						      name
						    | [iter] => ExpProcess.exp2term
								    (ExpProcess.prependIteratorToSymbol iter (Exp.TERM name))
						    | rest => (* this is an error *)
						      DynException.stdException(("Particular output '"^(e2s (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym,_)=> Symbol.name sym) temporal_iterators))),
										"ClassProcess.assignCorrectScope",
										Logger.INTERNAL)

					      (* now add the spatial iterators *)
					      val name'' = ExpProcess.exp2term 
							   (foldl (fn(iter,exp')=>ExpProcess.appendIteratorToSymbol iter exp') 
								  (Exp.TERM name')
								  spatial_iterators)
					  in
					      name''
					  end
				  (*val _ = Util.log("Converting name from '"^(e2s (Exp.TERM name))^"' to '"^(e2s (Exp.TERM name'))^"'")*)

			      in
				  {name=name',
				   contents=contents',
				   condition=condition'}
			      end
			   ) outputs
		      
	(* write back output changes *)
	val _ = (#outputs class) := outputs'
    in
	()
    end
    handle e => DynException.checkpoint "ClassProcess.assignCorrectScope" e

fun pruneClass iter_option (class: DOF.class) = 
    let
	(* pull out useful quantities *)
	val name = class2orig_name class
	val inputs = !(#inputs class)
	val input_syms = map (Term.sym2symname o #name) inputs (* grab the inputs just as symbols *)
	val outputs = !(#outputs class)
	val outputs' = case iter_option of
			   SOME (iter_sym,_) => List.filter (fn{name,...}=>ExpProcess.doesTermHaveIterator iter_sym (ExpProcess.term2exp name)) outputs
			 | NONE => outputs
	val output_symbols = Util.flatmap ExpProcess.exp2symbols (map #condition outputs' @ 
								  (Util.flatmap #contents outputs'))
	val exps = !(#exps class)

	(* starting dependency list - union of the outputs and possibly the states *)
	val dependency_list = SymbolSet.fromList output_symbols
	val state_list = case iter_option of
			     SOME (iter_sym,_) => class2statesbyiterator iter_sym class
			   | NONE => (*class2states class*)[]
	val dependency_list = SymbolSet.addList (dependency_list, state_list)

	(* do some magic here ... *)
	fun findDependencies dep_list = 
	    let
		val dep_list' = SymbolSet.fromList (Util.flatmap ExpProcess.exp2symbols (Util.flatmap (fn(sym)=> symbol2exps class sym) (SymbolSet.listItems dep_list)))
		(*val _ = Util.log("In class '"^(Symbol.name name)^"': " ^ (SymbolSet.toStr dep_list) ^ " -> " ^ (SymbolSet.toStr dep_list'))*)
	    in
		if SymbolSet.equal (dep_list, dep_list') then
		    dep_list (* dep_list hasn't changed, so we are done *)
		else
		    findDependencies (SymbolSet.union (dep_list, dep_list'))
	    end

	(* find all the dependencies *)
	val dependency_list = findDependencies dependency_list

	(* create a new set of expressions that filter out those that are not needed *)
	fun isStateEq exp = ExpProcess.isStateEq exp
	fun isStateEqOfValidIterator iter exp = ExpProcess.isStateEqOfIter iter exp
	fun isInitialConditionEq exp = ExpProcess.isInitialConditionEq exp
	fun isInitialConditionEqOfValidIterator (itersym,_) exp = (isInitialConditionEq exp) andalso (ExpProcess.doesEqHaveIterator itersym exp)
	val exps' = case iter_option of
			SOME iter => List.filter
					 (fn(exp)=> ((isStateEq exp) andalso (isStateEqOfValidIterator iter exp)) (* pull out all the exps that are state equations for that state with that iterator *)
						    orelse (* if it's not a state eq, check to see if the lhs defines what you are looking for ... *)
						    (isInitialConditionEqOfValidIterator iter exp)
						    orelse
						    ((not (isStateEq exp)) andalso (not (isInitialConditionEq exp)) andalso (SymbolSet.exists (fn(sym)=>List.exists (fn(sym')=> sym=sym') (ExpProcess.getLHSSymbols exp)) dependency_list)))
					 exps
		      | NONE => List.filter 
				    (fn(exp)=> SymbolSet.exists (fn(sym)=>List.exists (fn(sym')=> sym=sym') (ExpProcess.getLHSSymbols exp)) dependency_list) 
				    exps
		    
	(* check the inputs to see if any of them are not in the dependency list *)
	val _ = case iter_option of 
		    SOME _ => () (* don't worry about it here, it's really only important for the full pruning step *)
		  | NONE => 
		    let
			val input_sym_set = SymbolSet.fromList input_syms
			val dependencies_plus_inputs = SymbolSet.union (dependency_list, input_sym_set)
		    in
			if (SymbolSet.numItems (dependencies_plus_inputs)) > (SymbolSet.numItems dependency_list) then
			    Logger.log_warning (Printer.$("In class '"^(Symbol.name name)^"', one or more inputs are not used: " ^ (String.concatWith ", " (map Symbol.name (SymbolSet.listItems (SymbolSet.difference (dependency_list, dependencies_plus_inputs)))))))
			else
			    ()
		    end	       

    in
	((#exps class) := exps';
	 (#outputs class) := outputs')
    end

(*
fun pruneClassHelper keepstates prunedClasses (class: DOF.class)) = 
    let	
	val kept_symbols = outputs @ (if keepstates then states else [])
	val dependency_list = SymbolSet.fromList kept_symbols

	fun find_dependencies exps depList =
	    

	fun exp2data exp = 
	    (exp, lhs_symbols exp, rhs_symbols exp)
	val exp_data = map exp2data exps
	fun findSource sym = exp
	fun findDependencies exp = sym list

	(* assuming we have the dependencies, filter out those equations without them *)
	fun keep_exp_data (exp, lhs, rhs) = List.length (Util.intersection (lhs, required_syms)) > 0
	val (kept_exps, removed_exps) = List.partition keep_exp_data exp_data

	(* find required inputs *)
	fun keep_input input = List.exists (fn(sym)=>sym=input) required_syms
	val (kept_inputs, removed_inputs) = List.partition keep_input inputs

	(* find all instances that are left, and propagate through them *)
	val kept_insts = List.filter isInst kept_exps
	val kept_classes = Util.uniquify inst2class kept_insts
	val finished_classes = map (pruneClassHelper keepstates prunedClasses) kept_classes
	(*val finished_classes = foldl (fn(c, (classlist, prunedClassList))=> 
					  let
					      val name = #name class
					      val (class', prunedClasses') = pruneClassHelper keepstates prunedClasses c
					  in
					      (class'
					  end) [] kept_classes*)

    in
	(*return the new class*)
	class'
    end*)

fun optimizeClass (class: DOF.class) =
    let
	val exps = !(#exps class)

	(* first, convert all subs to adds and divs to recip *)
	val exps' = map (fn(exp)=> Match.applyRewritesExp [Rules.replaceSubWithNeg,
							   Rules.replaceDivWithRecip] exp) exps

	val _ = print "Round 2!\n"
	(* next, aggregate all additions *)
	val exps'' = map (fn(exp)=> Match.repeatApplyRewritesExp [Rules.distributeNeg,
								  Rules.aggregateSums1,
								  Rules.aggregateSums2,
								  Rules.aggregateProds1,
								  Rules.aggregateProds2,
								  Rules.aggregateProds] exp) exps'

	val _ = (#exps class) := exps''
    in
	()
    end
    

end
