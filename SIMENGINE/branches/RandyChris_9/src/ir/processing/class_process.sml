signature CLASSPROCESS =
sig

    (* Optimization method - primary method used to call optimizations that get executed across a class. *)
    (* Note: All class optimizations work on expressions that are references, therefore most methods don't have a return value *)
    val optimizeClass : DOF.class -> unit

    (* Accessor methods *)    
    val class2instnames : DOF.class -> (Symbol.symbol * Symbol.symbol) list (* return a list of instances within the class as classname/instname tuples *)
    val class2orig_name : DOF.class -> Symbol.symbol (* the name of the class before it was renamed, for example during ordering *)
    val class2classname : DOF.class -> Symbol.symbol (* returns the class name as stored in the structure, not the "realclassname" *)
    val class2basename : DOF.class -> Symbol.symbol (* the name generated for the class in modeltranslate - this is never adjusted *)
    val findSymbols : DOF.class -> SymbolSet.set (* return a list of every unique symbol name used in the class - helpful for global renaming *)
    val class2states : DOF.class -> Symbol.symbol list (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesbyiterator : Symbol.symbol -> DOF.class -> Symbol.symbol list (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesize : DOF.class -> int (* returns the total number of states stored in the class *)
    val class2statesizebyiterator : DOF.systemiterator -> DOF.class -> int (* limit the state count to those associated with a particular temporal iterator *)
    val class2instancesbyiterator : Symbol.symbol -> DOF.class -> Exp.exp list (* grab all the instances that have states matching the specified iterator, determined by calling class2statesizebiterator *)
    val symbol2exps : DOF.class -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val symbolofiter2exps :  DOF.class -> Symbol.symbol -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val class2update_states : DOF.class -> Symbol.symbol list (* find all state names that have an update equation associated *)

    (* Indicates whether a class is a functional form. *)
    val isInline : DOF.class -> bool
    (* Indicates whether a class is a master class. *)
    val isMaster : DOF.class -> bool

    (* Functions to modify class properties, usually recursively through the expressions *)
    val duplicateClass : DOF.class -> Symbol.symbol -> DOF.class (* makes a duplicate class with the new supplied name *)
    val updateRealClassName : DOF.class -> Symbol.symbol -> DOF.class 
    val pruneClass : DOF.systemiterator option -> DOF.class -> unit (* prunes unneeded equations in the class, the initial bool causes all states to be kept as well *)
    val propagateSpatialIterators : DOF.class -> unit (* propagates iterators through equations into outputs *)
    val assignCorrectScope : DOF.class -> unit (* sets read state or write state properties on symbols *)
    val updateForkedClassScope : DOF.systemiterator -> DOF.class -> unit (* update the scopes on symbols for those reads that are to be read from a per-iterator state structure instead of the system state structure *)
    val addEPIndexToClass : bool -> DOF.class -> unit (* sets the embarrassingly parallel property on symbols in all but the top level class *)
    val makeSlaveClassProperties : DOF.classproperties -> DOF.classproperties (* updates a class to make it a slave class - this is one that shouldn't write any states but can generate intermediates *)
    val fixSymbolNames : DOF.class -> unit (* makes all symbol names C-compliant *)
    val sym2codegensym : Symbol.symbol -> Symbol.symbol (* helper function used by fixSymbolNames to update just one symbol *)
    type sym = Symbol.symbol
    val renameInsts :  ((sym * sym) * (sym * sym)) -> DOF.class -> unit (* change all instance names in a class *)
    val createEventIterators : DOF.class -> unit (* searches out postprocess and update iterators *)

end
structure ClassProcess : CLASSPROCESS = 
struct

type sym = Symbol.symbol
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

fun updateRealClassName (class: DOF.class) new_name =
    let
	val {name, properties={sourcepos,basename,classform,classtype}, inputs, outputs, exps, iterators} = class
	val classtype' = case classtype of
			      DOF.MASTER _ => DOF.MASTER new_name
			    | DOF.SLAVE _ => DOF.SLAVE new_name
    in
	{name=name,
	 iterators=iterators,
	 properties={sourcepos=sourcepos,
		     basename=basename, (* this remains the same as it was previously defined in modeltranslate *)
		     classform=classform,
		     classtype=classtype'}, (* this is updated *)
	 inputs=inputs,
	 outputs=outputs,
	 exps=exps}
    end	

fun renameInsts (syms as ((name, new_name),(orig_name, new_orig_name))) (class: DOF.class) =
    let	
	(*val _ = Util.log("in ClassProcess.renameInsts: class=" ^ (Symbol.name (#name class)) ^ ", name="^(Symbol.name name)^" ,new_name=" ^ (Symbol.name new_name))*)
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val rename = ExpProcess.renameInst syms
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

	fun exp2symbolset exp =
	    ExpProcess.exp2symbolset exp

	fun input2symbols (inp as {name, default}) =
	    SymbolSet.fromList (ExpProcess.exp2symbols (Exp.TERM (name)) @ 
				(case default of
				     SOME v => ExpProcess.exp2symbols v
				   | NONE => []))

	fun output2symbols (out as {name, contents, condition}) =
	    SymbolSet.unionList [(exp2symbolset (Exp.TERM name)),
				 (SymbolSet.flatmap exp2symbolset contents),
				 (exp2symbolset condition)]	    

    in
	SymbolSet.unionList [SymbolSet.flatmap input2symbols inputs,
			     (SymbolSet.flatmap output2symbols outputs),
			     (SymbolSet.flatmap exp2symbolset exps)]
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
	SymbolSet.app 
	    (fn(sym)=>
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

fun class2basename (class : DOF.class) = 
    let
	val {properties={basename,...},...} = class
    in
	basename
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
    List.filter 
	(fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) 
	(!(#exps class))

fun symbolofiter2exps (class: DOF.class) iter_sym sym =
    List.filter 
	(fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) 
	(List.filter (ExpProcess.doesEqHaveIterator iter_sym) (!(#exps class)))

fun symbolofoptiter2exps (class: DOF.class) iter_sym sym =
    List.filter 
	(fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) 
	(List.filter (fn(exp)=> (ExpProcess.doesEqHaveIterator iter_sym exp) orelse 
				(case ExpProcess.exp2temporaliterator exp of NONE => true | _ => false)) (!(#exps class)))

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

fun isInline ({properties={classform=DOF.FUNCTIONAL,...},...} : DOF.class) = true
  | isInline _ = false

fun isMaster ({properties={classtype=DOF.MASTER _,...},...} : DOF.class) = true
  | isMaster _ = false

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
	    let
		val lhs = ExpProcess.lhs exp

		val spatial = ExpProcess.exp2spatialiterators exp
		val init_eq = case List.find (ExpProcess.isInitialConditionEq) (symbol2exps class (ExpProcess.getLHSSymbol exp)) of
				  SOME eq => eq
				| NONE => DynException.stdException ("Unexpected lack of initial condition", "ClassProcess.createEventIterators.update_exp", Logger.INTERNAL)
		val temporal = case ExpProcess.exp2temporaliterator (init_eq) of
				   SOME v => #1 v
				 | _ => DynException.stdException ("Unexpected init condition w/o temporal iterator", "ClassProcess.createEventIterators.update_exp", Logger.INTERNAL)

		val lhs' = 		
		    case lhs of 
			Exp.TERM (Exp.SYMBOL (sym, props)) => 
			Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props ((Iterator.updateOf (Symbol.name temporal),(*iterindex*)Iterator.RELATIVE 1)::spatial)))
		      | _ => DynException.stdException ("Non symbol on left hand side of intermediate", "ClassProcess.createEventIterators.update_exp", Logger.INTERNAL)
			     
	    in
		ExpBuild.equals (lhs', ExpProcess.rhs exp)
	    end			  

	fun pp_exp exp = 
	    let
		val lhs = ExpProcess.lhs exp

		val spatial = ExpProcess.exp2spatialiterators exp
		val init_eq = case List.find (ExpProcess.isInitialConditionEq) (symbol2exps class (ExpProcess.getLHSSymbol exp)) of
				  SOME eq => eq
				| NONE => DynException.stdException ("Unexpected lack of initial condition", "ClassProcess.createEventIterators.pp_exp", Logger.INTERNAL)

		val temporal = case ExpProcess.exp2temporaliterator (init_eq) of
				   SOME v => #1 v
				 | _ => DynException.stdException ("Unexpected init condition w/o temporal iterator", "ClassProcess.createEventIterators.pp_exp", Logger.INTERNAL)
		val lhs' = 
		    case lhs of 
			Exp.TERM (Exp.SYMBOL (sym, props)) => 
			if ExpProcess.isInitialConditionEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props ((Iterator.postProcessOf (Symbol.name temporal),(*iterindex*)Iterator.ABSOLUTE
																		   0)::spatial)))
			else if ExpProcess.isIntermediateEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, Property.setIterator props ((Iterator.postProcessOf (Symbol.name temporal),(*iterindex*)Iterator.RELATIVE
																		   1)::spatial)))
			else
			    DynException.stdException ("Unexpected non-intermediate and non-initial condition equation",
						       "ClassProcess.createEventIterators.pp_exp", Logger.INTERNAL)
		      | _ => DynException.stdException ("Non symbol on left hand side of intermediate", "ClassProcess.createEventIterators.pp_exp", Logger.INTERNAL)
	    in
		ExpBuild.equals (lhs', ExpProcess.rhs exp)
	    end			  

	val update_states = class2update_states class
	val postprocess_states = class2postprocess_states class

	(* update all the intermediate equations based on the initial condition iterators *)
	val exps' = map
			(fn(exp)=>if ExpProcess.isIntermediateEq exp then
				      let
					  val lhs_sym = ExpProcess.getLHSSymbol exp
				      in
					  if List.exists (fn(sym)=>sym=lhs_sym) update_states then
					      update_exp exp
					  else if List.exists (fn(sym)=>sym=lhs_sym) postprocess_states then
					      pp_exp exp
					  else
					      exp
				      end
				  else
				      exp)
			exps

	(* for post process iterators, we also have to update the initial condition *)
	val exps'' = map
			(fn(exp)=>if ExpProcess.isInitialConditionEq exp then
				      let
					  val lhs_sym = ExpProcess.getLHSSymbol exp
				      in
					  if List.exists (fn(sym)=>sym=lhs_sym) postprocess_states then
					      pp_exp exp
					  else
					      exp
				      end
				  else
				      exp)
			exps'

    in
	(#exps class := exps'')
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

	(*val _ = Util.log ("In class2instname: all_classes={"^(String.concatWith ", " (map (fn(c)=> (Symbol.name (#name c) ^ ":" ^ (Symbol.name (name2orig_name (#name c))))) all_classes))^"}")*)

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

(* just see if there are states that use this iterator...  there could be reads, but that's not an issue *)
fun class_has_iterator (iter: DOF.systemiterator) (class: DOF.class) =
    let	
	val (iter_sym, _) = iter
	val class_states = class2statesbyiterator iter_sym class

	val instance_equations = List.filter ExpProcess.isInstanceEq (!(#exps class))

	(*val _ = Util.log ("in class2statesizebyiterator for class '"^(Symbol.name name)^"', # of init conditions="^(i2s (List.length initial_conditions))^", # of instances=" ^ (i2s (List.length instance_equations)))*)
    in
	(List.length class_states) > 0 orelse
	(StdFun.listOr (map (fn(exp)=> 
			       let
				   val {classname,...} = ExpProcess.deconstructInst exp
			       in
				   class_has_iterator iter (CurrentModel.classname2class classname)
			       end
			    ) instance_equations))
    end

fun class2statesizebyiterator (iter: DOF.systemiterator) (class: DOF.class) =
    let	
	val {name,exps,iterators,...} = class
	val initial_conditions = 
	    case iter of
		(iter_sym, DOF.UPDATE dyn_iter) => 
		let
		    val init_eqs = List.filter ExpProcess.isInitialConditionEq (!exps)
		    val dyn_iter_init_eqs = List.filter (ExpProcess.doesEqHaveIterator dyn_iter) init_eqs
		    val update_states = class2statesbyiterator iter_sym class
		    (*val _ = Util.log ("Computing initial_conditions: # of init_eqs: "^(i2s (List.length init_eqs))^", # of dyn_iter_init_eqs: "^(i2s (List.length dyn_iter_init_eqs))^", # of update_states: "^ (i2s (List.length update_states)))*)
		in
		    List.filter (fn(eq)=>List.exists (fn(sym)=>ExpProcess.getLHSSymbol eq = sym) update_states) dyn_iter_init_eqs
		end
	      | (iter_sym,_) => List.filter (ExpProcess.doesEqHaveIterator iter_sym) (List.filter ExpProcess.isInitialConditionEq (!exps))

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


fun class2instancesbyiterator iter_sym class =
    let
	val instances = class2instances class
	val iter = CurrentModel.itersym2iter iter_sym
    in
	List.filter 
	    (fn(inst)=>
	       let
		   val {classname,...} = ExpProcess.deconstructInst inst
		   val class' = CurrentModel.classname2class classname
		   (*val _ = Util.log ("Found class '"^(Symbol.name (#name class'))^"' to have iterators? " ^ (Util.b2s (class_has_iterator iter class')))*)
	       in
		   class_has_iterator iter class'
	       end)
	    instances
    end
	
fun makeSlaveClassProperties props = 
    let
	val {classtype, classform, sourcepos, basename} = props
    in
	{classtype=case classtype of
		       DOF.MASTER classname => DOF.SLAVE classname
		     | _ => classtype,
	 classform=classform,
	 sourcepos=sourcepos,
	 basename=basename}
    end


(* this will propagate an iterator from an input to an output *)
fun propagateSpatialIterators (class: DOF.class) =
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
										       "ClassProcess.propagateSpatialIterators",
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

(* takes a symbol name, finds an equation and returns all the iterators *)
fun sym2iterators (class: DOF.class) sym =
    let
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
    in
	(temporal_iterators, spatial_iterators)
    end

fun assignCorrectScope (class: DOF.class) =
    let
	val exps = !(#exps class)

	val state_equations = List.filter ExpProcess.isStateEq exps
	val state_terms = map ExpProcess.lhs state_equations
	val symbols = map (Term.sym2curname o ExpProcess.exp2term) state_terms
	val state_iterators_options = map (TermProcess.symbol2temporaliterator o ExpProcess.exp2term) state_terms
	val iterators = CurrentModel.iterators()
	val indexable_iterators = List.mapPartial (fn(iter_sym, iter_type)=> case iter_type of
										 DOF.CONTINUOUS _ => SOME iter_sym
									       | DOF.DISCRETE _ => SOME iter_sym
									       | _ => NONE) iterators
	val symbol_state_iterators = 
	    let
		val all_state_iterators = 
		    map (fn(exp, iter)=>
			   case iter of
			       SOME i => i
			     | NONE => DynException.stdException(("State '"^(e2s exp)^"' does not have temporal iterator associated with it"), "ClassProcess.assignCorrectScope", Logger.INTERNAL))
			(ListPair.zip (state_terms, state_iterators_options))

		(* we have to prune update iterators *)
		fun isUpdateIterator (iter_sym, _) =
		    List.exists (fn(iter_sym', iter_type)=> case iter_type of
								DOF.UPDATE _ => iter_sym = iter_sym'
							      | _ => false) iterators
	    in
		List.filter (fn(_, iter)=> not (isUpdateIterator iter)) (ListPair.zip (symbols, all_state_iterators))
	    end


	(*val _ = Util.log ("In assignCorrectScope, producing rewriting rules: ")
	val _ = app (fn(sym, (iter_sym,_))=> Util.log (" -> sym: " ^ (Symbol.name sym) ^ ", iter: " ^ (Symbol.name iter_sym))) symbol_state_iterators*)
	(*Util.flatmap ExpProcess.exp2symbols state_terms*)
	val state_actions = map 
			  (fn(sym, iter as (itersym, _))=>
			     {find=Match.asym sym, 
			      test=NONE, 
			      replace=Rewrite.ACTION (Symbol.symbol (Symbol.name sym ^ "["^(Symbol.name (#1 iter))^"]"),
						      (fn(exp)=>ExpProcess.assignCorrectScopeOnSymbol
								    (ExpProcess.prependIteratorToSymbol itersym exp)))}) 
			  symbol_state_iterators

	val iter_actions = map
			       (fn(sym)=>
				  {find=Match.asym sym,
				   test=NONE,
				   replace=Rewrite.ACTION (sym,
							   (fn(exp)=> 
							      case exp of
								  Exp.TERM (Exp.SYMBOL (sym', props))=> 
								  Exp.TERM (Exp.SYMBOL (sym', 
											Property.setScope 
											    props 
											    Property.ITERATOR))
									| _ => DynException.stdException
										   ("Unexpected expression",
										    "ClassProcess.assignCorrectScope.iter_actions", 
										    Logger.INTERNAL)))})
			       indexable_iterators
	val actions = state_actions @ iter_actions

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
					      val sym = Term.sym2curname name
					      val (temporal_iterators, spatial_iterators) = sym2iterators class sym

					      (* transform pp[x] and update[x] into x references*)
					      val temporal_iterators' = foldl (fn(iterator, iterators) =>
										 let
										     val globaliterators = CurrentModel.iterators()
										     val iterator' =
											 case List.find (fn(s,_) => s = iterator) globaliterators of
											     SOME (_, itertype) =>
											     (case itertype of
												  DOF.POSTPROCESS iter => iter
												| DOF.UPDATE iter => iter
												| _ => iterator)
											   | NONE => DynException.stdException("No global iterator found for temporal iterator " ^ (Symbol.name iterator),
															       "ClassProcess.assignCorrectScope.outputs'",
															       Logger.INTERNAL)

										     val iterators' = if List.exists (fn(s) => s = iterator') iterators then
													  iterators
												      else
													  iterator' :: iterators
										 in
										     iterators'
										 end) 
									      nil 
									      (map #1 temporal_iterators)

					      (* assign the temporal iterator first *)
					      val name' = 
						  case temporal_iterators' of
						      [] => (* no iterators present, just return name *)			
						      name
						    | [iter] => ExpProcess.exp2term
								    (ExpProcess.prependIteratorToSymbol iter (Exp.TERM name))
						    | rest => (* this is an error *)
						      DynException.stdException(("Particular output '"^(e2s (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym)=> Symbol.name sym) temporal_iterators'))),
										"ClassProcess.assignCorrectScope",
										Logger.INTERNAL)

					      (* now add the spatial iterators *)
					      val name'' = ExpProcess.exp2term 
							   (foldl (fn(iter as (itersym,_),exp')=>ExpProcess.appendIteratorToSymbol itersym exp') 
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

fun updateForkedClassScope (iter as (iter_sym, iter_type)) (class: DOF.class) =
    let
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val new_scope = Property.READSTATE (Symbol.symbol (case iter_type of 
							       DOF.UPDATE v => "rd_" ^ (Symbol.name v)
							     | _ => "rd_" ^ (Symbol.name iter_sym)))

	val pred = ("CheckingForScope",(fn(exp)=>
					  case exp of
					      Exp.TERM (Exp.SYMBOL (sym, props)) => 
					      (case Property.getScope props of
						   (Property.READSYSTEMSTATE v) => v = iter_sym
						 | _ => false)
					    | _ => false))

	val find = Match.anysym_with_predlist [pred] (Symbol.symbol "a")
	val action = (fn(exp)=>
			case exp of 
			    Exp.TERM (Exp.SYMBOL (sym, props)) => Exp.TERM (Exp.SYMBOL (sym, Property.setScope props new_scope))
			  | _ => exp)
	val rewrite = {find=find, test=NONE, replace=Rewrite.ACTION (Symbol.symbol ("UpdateForkedClassIterTo:"^(Symbol.name iter_sym)),action)}
	val exps' = map (Match.applyRewriteExp rewrite) exps
	val outputs' = map (fn{name, contents, condition}=>
			      {name=name, (* shouldn't have to update this term *)
			       contents=map (Match.applyRewriteExp rewrite) contents,
			       condition=Match.applyRewriteExp rewrite condition}) outputs

    in
	(#exps class := exps';
	 #outputs class := outputs')
    end

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

	(* now add the states if need be *)
	val state_list = case iter_option of
			     SOME (iter_sym,_) => class2statesbyiterator iter_sym class
			   | NONE => (*class2states class*)[]
	val dependency_list = SymbolSet.addList (dependency_list, state_list)

	(* add all the instances as well that have states (again, if need be) *)
	val instance_list = case iter_option of
			     SOME (iter_sym,_) => class2instancesbyiterator iter_sym class
			   | NONE => []
	val instance_dependencies = SymbolSet.flatmap ExpProcess.exp2symbolset instance_list
	val dependency_list = SymbolSet.union (dependency_list, instance_dependencies)
				     

	(* do some magic here ... *)
	fun findDependencies dep_list = 
	    let
		val dep_list' = 
		    case iter_option of 
			SOME (iter_sym, _) => SymbolSet.flatmap ExpProcess.exp2symbolset (Util.flatmap (fn(sym)=> symbolofoptiter2exps class iter_sym sym) (SymbolSet.listItems dep_list))
		      | NONE => SymbolSet.flatmap ExpProcess.exp2symbolset (Util.flatmap (fn(sym)=> symbol2exps class sym) (SymbolSet.listItems dep_list))
(*		val _ = case iter_option of
			    SOME (iter_sym,_) => Util.log("In class '"^(Symbol.name name)^"' (iterator="^(Symbol.name iter_sym)^"): " ^ (SymbolSet.toStr dep_list) ^ " -> " ^ (SymbolSet.toStr dep_list'))
			  | NONE => Util.log("In class '"^(Symbol.name name)^"': " ^ (SymbolSet.toStr dep_list) ^ " -> " ^ (SymbolSet.toStr dep_list'))*)
		(* add any remaining iterators that weren't there before *)
		val dep_list' = SymbolSet.union (dep_list, dep_list')
	    in
		if SymbolSet.equal (dep_list, dep_list') then
		    dep_list (* dep_list hasn't changed, so we are done *)
		(*else if (SymbolSet.numItems dep_list) > (SymbolSet.numItems dep_list') then (* if we lost symbols, but just add them in ... *)
		    DynException.stdException(("Unexpected loss of symbols: " ^ (case iter_option of
			    SOME (iter_sym,_) => "in class '"^(Symbol.name name)^"' (iterator="^(Symbol.name iter_sym)^"): " ^ (SymbolSet.toStr dep_list) ^ " -> " ^ (SymbolSet.toStr dep_list')
			  | NONE => "in class '"^(Symbol.name name)^"': " ^ (SymbolSet.toStr dep_list) ^ " -> " ^ (SymbolSet.toStr dep_list'))), "ClassProcess.pruneClass.findDependencies", Logger.INTERNAL)*)
		else
		    findDependencies ((*SymbolSet.union (dep_list, dep_list')*)dep_list')
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
