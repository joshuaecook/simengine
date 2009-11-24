signature CLASSPROCESS =
sig
    val unify : DOF.class -> DOF.class

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
    (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesbyiterator : Symbol.symbol -> DOF.class -> Symbol.symbol list 
    val class2statesize : DOF.class -> int (* returns the total number of states stored in the class *)
    val class2statesizebyiterator : DOF.systemiterator -> DOF.class -> int (* limit the state count to those associated with a particular temporal iterator *)
    val class2instancesbyiterator : Symbol.symbol -> DOF.class -> Exp.exp list (* grab all the instances that have states matching the specified iterator, determined by calling class2statesizebiterator *)
    val symbol2exps : DOF.class -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val symbolofiter2exps :  DOF.class -> Symbol.symbol -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val class2update_states : DOF.class -> Symbol.symbol list (* find all state names that have an update equation associated *)

    val outputsByIterator : DOF.systemiterator -> DOF.class -> DOF.output list

    val outputsSymbols : DOF.class -> Exp.term list
    val outputsSymbolsByIterator : DOF.systemiterator -> DOF.class -> Exp.term list

    val findInput : DOF.class -> Symbol.symbol -> DOF.input option

    (* Indicates whether a class is a functional form. *)
    val isInline : DOF.class -> bool
    (* Indicates whether a class is a master class. *)
    val isMaster : DOF.class -> bool
    (* Indicates whether a class has a non-zero number of states. *)
    val hasStates : DOF.class -> bool
    (* Indicates whether a class has instances. *)
    val hasInstances : DOF.class -> bool
    (* Indicates whether a class contains states associated with a given iterator. *)
    val hasStatesWithIterator : DOF.systemiterator -> DOF.class -> bool
    val requiresIterator : DOF.systemiterator -> DOF.class -> bool

    (* Functions to modify class properties, usually recursively through the expressions *)
    val applyRewritesToClass : Rewrite.rewrite list -> DOF.class -> unit (* generic rewriting helper *)
    val duplicateClass : DOF.class -> Symbol.symbol -> DOF.class (* makes a duplicate class with the new supplied name *)
    val updateRealClassName : DOF.class -> Symbol.symbol -> DOF.class 
    val pruneClass : (DOF.systemiterator option * bool) -> DOF.class -> unit (* prunes unneeded equations in the class, the initial bool causes all states to be kept as well *)
    val propagateSpatialIterators : DOF.class -> unit (* propagates iterators through equations into outputs *)
    val assignCorrectScope : DOF.class -> unit (* sets read state or write state properties on symbols *)
    val updateForkedClassScope : DOF.systemiterator -> DOF.class -> unit (* update the scopes on symbols for those reads that are to be read from a per-iterator state structure instead of the system state structure *)
    val addEPIndexToClass : bool -> DOF.class -> unit (* sets the embarrassingly parallel property on symbols in all but the top level class *)
    val makeSlaveClassProperties : DOF.classproperties -> DOF.classproperties (* updates a class to make it a slave class - this is one that shouldn't write any states but can generate intermediates *)
    val fixSymbolNames : DOF.class -> unit (* makes all symbol names C-compliant *)
    type sym = Symbol.symbol
    val renameInsts :  ((sym * sym) * (sym * sym)) -> DOF.class -> unit (* change all instance names in a class *)
    val createEventIterators : DOF.class -> unit (* searches out postprocess and update iterators *)
    val addDelays : DOF.class -> unit (* adds delays to difference equation terms *)
    val addBufferedIntermediates : DOF.class -> unit (* for iterators that read and write to the same state vector, so we add intermediates to break up any loops *)

    val to_json : DOF.class -> mlJS.json_value
end
structure ClassProcess : CLASSPROCESS = 
struct

type sym = Symbol.symbol
val i2s = Util.i2s
val e2s = ExpPrinter.exp2str

fun equal a b = a = b

fun applyRewritesToClass rewrites (class:DOF.class) =
    let
	val inputs = !(#inputs class)
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val inputs' = map (fn{name, default}=>{name=ExpProcess.exp2term (Match.applyRewritesExp rewrites (ExpProcess.term2exp name)),
					       default=case default of 
							   SOME exp => SOME (Match.applyRewritesExp rewrites exp)
							 | NONE => NONE}) inputs
	val exps' = map (Match.applyRewritesExp rewrites) exps

	val outputs' = map (fn{name,contents,condition}=>
			      {name=ExpProcess.exp2term (Match.applyRewritesExp rewrites (ExpProcess.term2exp name)),
			       contents=map (Match.applyRewritesExp rewrites) contents,
			       condition=Match.applyRewritesExp rewrites condition})
			   outputs
		      
    in
	((#inputs class):=inputs';
	 (#exps class):=exps';
	 (#outputs class):=outputs')
    end

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

fun findInput class sym =
    let val {inputs, ...} : DOF.class = class
    in List.find (fn {name, ...} => Term.sym2curname name = sym) (!inputs)
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

fun flattenExpressionThroughInstances class exp =
    let val symbols = ExpProcess.exp2symbols exp
	val equations = map (flattenEquationThroughInstances class) symbols
	val rules = map ExpProcess.equation2rewrite equations
    in
	Match.applyRewritesExp rules exp
    end
    handle e => DynException.checkpoint "ClassProcess.flattenExpressionThroughInstances" e

and flattenEquationThroughInstances class sym =
    let val {name=classname, inputs, ...} = class
    in if isSymInput class sym then
	ExpBuild.equals (ExpBuild.var (Symbol.name sym), 
			 Exp.TERM (#name (valOf (List.find (fn {name, ...} => Term.sym2curname name = sym) (! inputs)))))
       else
	   case findMatchingEq class sym
	    of SOME exp =>
	       if ExpProcess.isInstanceEq exp then
		   flattenInstanceEquation class sym exp
	       else if ExpProcess.isIntermediateEq exp then
		   let val locals = List.filter Term.isLocal (ExpProcess.exp2termsymbols (ExpProcess.rhs exp))
		       (* val _ = Util.log ("Found matching eq for sym '"^(Symbol.name sym)^"' -> '"^(e2s exp)^"'") *)
		   in if 1 = List.length locals andalso sym = Term.sym2curname (List.hd locals) then
			  exp
		      else
			  let fun make_rule term =
				  let val replace = 
					  case flattenEquationThroughInstances class (Term.sym2curname term)
					   of exp => if ExpProcess.isEquation exp then ExpProcess.rhs exp else exp
				  in {find = Exp.TERM term, replace = Rewrite.RULE replace, test = NONE} 
				  end
			  in Match.applyRewritesExp (map make_rule locals) exp
			  end
		   end
	       else 
		   exp
	     | NONE => DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenEquationThroughInstances", Logger.INTERNAL)
    end

(* Constructs a flattened expression by associating the given symbol with an output parameter,
 * then inspecting the instance class to find the class output expression. *)
and flattenInstanceEquation caller sym eqn =
    let val {classname, outargs, inpargs, ...} = ExpProcess.deconstructInst eqn
	val class = CurrentModel.classname2class classname
	val {outputs, inputs, ...} = class
	val output_ix = case List.find (fn (x,_) => Term.sym2curname x = sym) (Util.addCount outargs)
			 of SOME (_, index) => index
			  | NONE => 
			    DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenInstanceEquation", Logger.INTERNAL)
	val {name, contents, condition} = List.nth (! outputs, output_ix)

	(* val _ = Util.log ("flattenInstanceEquation for sym '"^(Symbol.name sym)^"': '"^(e2s eqn)^"'") *)
    in case TermProcess.symbol2temporaliterator name
	of SOME _ => ExpBuild.equals (ExpBuild.var (Symbol.name sym), Exp.TERM name)
	 | NONE => 
	   let val terms = 
		   List.concat (map (ExpProcess.exp2termsymbols o (flattenExpressionThroughInstances class)) (condition :: contents))
	       val terms = 
		   Util.flatmap (fn t => 
		   if isSymInput class (Term.sym2curname t) then
		       let val input_ix = case List.find (fn ({name, ...}, _) => Term.sym2curname name = Term.sym2curname t) (Util.addCount (! inputs))
					   of SOME (_, index) => index
					    | NONE => 
					      DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenInstanceEquation", Logger.INTERNAL)
			   val inp = List.nth (inpargs, input_ix)
		       in ExpProcess.exp2termsymbols (flattenExpressionThroughInstances caller inp)
		       end
		   else [t]
	           ) terms
	   in		   
	       ExpBuild.equals (ExpBuild.var (Symbol.name sym), 
				Exp.TERM (if 1 = List.length terms then List.hd terms else Exp.TUPLE terms))
	   end
    end


(* flattenEq does not pass through instance equations - we need a different one that will pass through instance equations *)
fun flattenEq (class:DOF.class) sym = 
    if isSymInput class sym then
	Exp.TERM (#name (valOf (List.find (fn{name,...}=>Term.sym2curname name = sym) (!(#inputs class)))))
    else
	case findMatchingEq class sym of
	    SOME exp => 
	    let
		(* val _ = Util.log ("Found matching eq for sym '"^(Symbol.name sym)^"' -> '"^(e2s exp)^"'") *)
		val symbols = ExpProcess.exp2termsymbols (ExpProcess.rhs exp)
		val local_symbols = List.filter Term.isLocal symbols
		val matching_equations = map ((findMatchingEq class) o Term.sym2curname) local_symbols
		(* only use symbols that are from intermediates *)
		val filtered_symbols = map #1 
					   (List.filter 
						(fn(_,equ)=> case equ of 
								 SOME e => not (ExpProcess.isInstanceEq e)
							       | NONE => false)
						(ListPair.zip (local_symbols,matching_equations)))


		fun term_rewrite_rule term = 
		    let
			val find = Exp.TERM term
			val test = NONE
			(* val _ = Util.log ("flattenEq ("^(Symbol.name (#name class))^"): '"^(e2s find)^"'") *)
			val repl_exp = ExpProcess.rhs (flattenEq class (Term.sym2curname term))
			(* val _ = Util.log (" ->  '"^(e2s repl_exp)^"'") *)
			val replace = Rewrite.RULE repl_exp
		    in
			{find=find, test=test, replace=replace}			       
		    end
	    in
		if 1 = List.length filtered_symbols andalso sym = (Term.sym2curname (List.hd filtered_symbols)) then exp
		else Match.applyRewritesExp (map term_rewrite_rule filtered_symbols) exp
	    end
	  | NONE => DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenEq", Logger.INTERNAL)


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
					      Term.isReadSystemState t orelse
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

val parallelSuffix = "[modelid]"
    
(* fix according to C rules *)
fun fixSymbolNames (class: DOF.class) =
    let
	val symbols = findSymbols class
	val iterators = CurrentModel.iterators()
	fun fixsym sym = if List.exists (fn(sym',_)=>sym=sym') iterators then
			     sym (* no need to fix 't' or another iterator - it's reserved *)
			 else
			     Util.sym2codegensym sym
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
	((List.exists (equal sym)) o ExpProcess.getLHSSymbols)
	(List.filter 
	     (fn (exp) => (ExpProcess.doesEqHaveIterator iter_sym exp) orelse 
			  (case ExpProcess.exp2temporaliterator exp of NONE => true | _ => false)) 
	     (!(#exps class)))

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

fun outputsByIterator iterator (class: DOF.class) =
    let val (iter_sym, _) = iterator
	val {outputs, ...} = class

	fun has_iterator ({name, ...}: DOF.output) =
	    let fun has exp =
		    case ExpProcess.exp2temporaliterator exp
		     of SOME (iter_sym', _) => iter_sym = iter_sym'
		      | _ => false
	    in has (ExpProcess.term2exp name)
	    end
    in List.filter has_iterator (! outputs)
    end

fun outputSymbols output =
    let val {contents, condition, ...} = output
    in
	Util.flatmap ExpProcess.exp2termsymbols (condition :: contents)
    end

fun outputSymbolsByFilter f output =
    List.filter f (outputSymbols output)

fun outputSymbolsByIterator iterator output =
    let val (iter_sym, _) = iterator
    in
	outputSymbolsByFilter
	    ((ExpProcess.doesTermHaveIterator iter_sym) o ExpProcess.term2exp)
	    output
    end

fun outputsSymbols class =
    let val {outputs, ...} = class
    in Util.flatmap outputSymbols (! outputs)
    end

fun outputsSymbolsByFilter f class =
    List.filter f (outputsSymbols class)

fun outputsSymbolsByIterator iterator (class : DOF.class) =
    let val (iter_sym, _) = iterator
    in
	outputsSymbolsByFilter
	    ((ExpProcess.doesTermHaveIterator iter_sym) o ExpProcess.term2exp)
	    class
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
		   in (hasInitEq exps') andalso (not (hasStateEq exps'))
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

	(* need to add the correct temporal iterators everywhere for pp iterators - this will propagate the pp_t to all the RHS terms that require it *)
	fun pp2rewrite (state_sym, (iter_sym,_)) = 
	    {find=Match.asym state_sym,
	     test=NONE,
	     replace=Rewrite.ACTION (Symbol.symbol ("pp_update:" ^ (Symbol.name state_sym)),
				     (fn(exp)=> ExpProcess.updateTemporalIteratorToSymbol (iter_sym, (fn(sym)=>sym)) exp))}

	val pp_rewrites = map (fn(exp)=>pp2rewrite (ExpProcess.getLHSSymbol exp, valOf (ExpProcess.exp2temporaliterator exp))) 
			      (List.filter ExpProcess.isPPEq exps'')

	val rewrite = Match.applyRewritesExp pp_rewrites
	val exps''' = map rewrite exps''
	val outputs' = map 
			   (fn{name,contents,condition}=>
			      {name=ExpProcess.exp2term (rewrite (ExpProcess.term2exp name)),
			       contents=map rewrite contents,
			       condition=rewrite condition})
			   outputs

    in
	(#exps class := exps''';
	 #outputs class := outputs')
    end
    handle e => DynException.checkpoint "ClassProcess.createEventIterators" e

(* find symbols of the form x[n-d] where d is an integer.. Then add d states to the system to create that particular value.  This should also work with x[t[-d]] *)
fun addDelays (class: DOF.class) =
    let
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val allTerms = (Util.flatmap ExpProcess.exp2termsymbols exps) @
		       (Util.flatmap (fn{name,contents,condition}=>(ExpProcess.exp2termsymbols condition) @
								   (Util.flatmap ExpProcess.exp2termsymbols contents)) outputs)

	val delayedTerms = List.filter (ExpProcess.isDelayedVarDifferenceTerm o ExpProcess.term2exp) allTerms

	fun term2name_delay (t as (Exp.SYMBOL (sym, _))) = 
	    (case TermProcess.symbol2temporaliterator t of
		 SOME (iter_sym, Iterator.RELATIVE d) => (sym, iter_sym, ~d)
	       | _ => DynException.stdException("ClassProcess.addDelays", "Unexpected non-relative iterator", Logger.INTERNAL))
	  | term2name_delay _ = DynException.stdException("ClassProcess.addDelays", "Unexpected non-symbol term", Logger.INTERNAL)

	val term_list = map term2name_delay delayedTerms

	val grouped_term_list = List.foldl (fn((sym,iter_sym,d),l)=>
					      let
						  val (match, others) = List.partition (fn(sym',_,_)=>sym=sym') l
					      in
						  if List.length match = 0 then
						      (sym, iter_sym, [d])::l
						  else 
						      let
							  val e as (sym',iter_sym',d_list) = Util.hd match
						      in
							  if List.exists (fn(d')=>d=d') d_list then
							      e::others
							  else
							      (sym',iter_sym',d::d_list)::others
						      end
					      end)
					   [] 
					   term_list
					   
	val exps_and_rewrites = 
	    map 
	    (fn(sym,iter_sym,d_list)=>
	       let
		   val max_d = StdFun.max d_list
		   val (init_condition_value, spatial_iterators) = case List.find (fn(exp)=> ExpProcess.isInitialConditionEq exp) (symbol2exps class sym) of
								       SOME exp => (ExpProcess.rhs exp, ExpProcess.exp2spatialiterators exp) 
								     | NONE => (ExpBuild.int 0,[])
									       
		   val pp_iter_sym = Iterator.postProcessOf (Symbol.name iter_sym)
		   fun d2symname d = 
		       Symbol.symbol ("#intdelay_" ^ (Symbol.name sym) ^ "_" ^ (Util.i2s d))
		   val init_conditions = map
					     (fn(d)=>ExpBuild.equals (Exp.TERM (Exp.SYMBOL (d2symname d, (Property.setIterator Property.default_symbolproperty ((Iterator.postProcessOf (Symbol.name iter_sym), Iterator.ABSOLUTE 0)::spatial_iterators)))), init_condition_value))
					     (List.tabulate (max_d,fn(x)=>x+1))

		   fun sym_props r = Property.setIterator Property.default_symbolproperty ((pp_iter_sym, Iterator.RELATIVE r)::spatial_iterators)
		   fun d2lhs_exp d r = Exp.TERM (Exp.SYMBOL (d2symname d, sym_props r))
		   val pp_equations = map
					  (fn(d)=>if d = 1 then 
						      ExpBuild.equals (d2lhs_exp d 1, 
								       Exp.TERM (Exp.SYMBOL (sym, 
											     Property.setIterator 
												 Property.default_symbolproperty 
												 ((iter_sym, Iterator.RELATIVE 0)::spatial_iterators))))
						  else
						      ExpBuild.equals (d2lhs_exp d 1,
								       d2lhs_exp (d-1) 0))
					  (List.tabulate (max_d,fn(x)=>x+1))
		   val rewrites = map
				      (fn(d)=>
					 let 
					     val pred = ("Find:" ^ (Symbol.name sym),
							 (fn(exp)=>
							    case exp of
								    Exp.TERM (Exp.SYMBOL (sym', props))=>sym=sym' andalso
													 (case ExpProcess.exp2temporaliterator exp of
													      SOME (iter_sym',Iterator.RELATIVE d') => iter_sym=iter_sym' andalso d=(~d')
													    | _ => false)
								  | _ => false))
					 in
					     {find=Match.anysym_with_predlist [pred] (Symbol.symbol "a"),
					      test=NONE,
					      replace=Rewrite.RULE (d2lhs_exp d 0)}
					 end
				      ) d_list
				  
	       in
		   (init_conditions @ pp_equations, rewrites)
	       end)
	    grouped_term_list

	val new_exps = StdFun.flatmap #1 exps_and_rewrites
	val rewrites = StdFun.flatmap #2 exps_and_rewrites

	val exps' = map (Match.applyRewritesExp rewrites) exps
	val outputs' = map (fn{name,contents,condition}=>
			      {name=name,
			       contents=map (Match.applyRewritesExp rewrites) contents,
			       condition=Match.applyRewritesExp rewrites condition})
			   outputs

    in
	(#exps class := (new_exps @ exps');
	 #outputs class := outputs')
    end
    handle e => DynException.checkpoint "ClassProcess.addDelays" e

(* for an update x[update_t+1] = x[update_t] - 1, for example, x[update_t] should really be x[update_t+1], and should be read from the same vector as x is written 
back to.  If there is coupling between multiple update equations {x=f(y), y=f(x)}, then we need to break the loop by inserting intermediates.  *)
(* The same is true for post processing.  This function handles both updates and post processing flows. *)
fun addBufferedIntermediates (class: DOF.class) = 
    let
	val expfilters = [ExpProcess.isUpdateEq, ExpProcess.isPPEq]
	fun addBufferedIntermediatesByType expfilter = 
	    let
		val exps = !(#exps class)
		val (filterEqs, restEqs) = List.partition expfilter exps

		(* find the filterEqs that have filtered iterator terms on the rhs *)
		val (dependentEqs, independentEqs) = 
		    List.partition
			(fn(exp)=>
			   let
			       (* this is the update iterator that we want to match against *)
			       val temporal_iterator = case (TermProcess.symbol2temporaliterator (ExpProcess.getLHSTerm exp)) of
							   SOME (sym, _) => sym
							 | NONE => DynException.stdException("Can't find temporal iterator in update eq",
											     "ClassProcess.addUpdateIntermediates", 
											     Logger.INTERNAL)
			       val rhs_terms = ExpProcess.exp2termsymbols (ExpProcess.rhs exp)
			   in
			       List.exists (fn(t)=> case (TermProcess.symbol2temporaliterator t) of
 							SOME (sym,_) => sym=temporal_iterator
						      | NONE => false) rhs_terms
			   end
			)
			filterEqs

		(* foreach of the dependentEqs, split them into two equations.  Ordering doesn't matter since there will be an ordering pass later... *)
		val splitDependentEqs = Util.flatmap 
						  (fn(eqs)=>
						     let
							 val lhs = ExpProcess.lhs eqs
							 val rhs = ExpProcess.rhs eqs
							 val gen_symbol = 
							     case ExpProcess.getLHSSymbols eqs of
								 [sym] => "#updateintermediate_" ^ (Symbol.name sym)
							       | nil => DynException.stdException(("Unexpectedly no symbols on lhs of expression " ^ (ExpPrinter.exp2str eqs)), 
												  "ClassProcess.addUpdateIntermediates", Logger.INTERNAL)
							       | _ => DynException.stdException(("Can not handle a tuple on lhs of update expression " ^ (ExpPrinter.exp2str eqs)), 
												"ClassProcess.addUpdateIntermediates", Logger.INTERNAL)
						     in
							 [ExpBuild.equals (ExpBuild.var gen_symbol, rhs),
							  ExpBuild.equals (lhs, ExpBuild.var gen_symbol)]
						     end) 
						  dependentEqs
	    in
		(#exps class) := (restEqs @ independentEqs @ splitDependentEqs)
	    end
    in
	app addBufferedIntermediatesByType expfilters
    end

fun addEPIndexToClass is_top (class: DOF.class) =
    let
	(*val _ = Util.log ("Adding EP Indices to class " ^ (Symbol.name (#name class)))*)
	val master_class = CurrentModel.classname2class (class2orig_name class)
	val states = findStateSymbols master_class
	(*val _ = Util.log ("Found States: " ^ (Util.symlist2s states))*)
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


fun hasStates class = 0 < class2statesize class
fun hasInstances (class:DOF.class) = List.length (List.filter ExpProcess.isInstanceEq (!(#exps class))) > 0

fun class2exps (class: DOF.class) =
    let
	val exps = !(#exps class)
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)
    in
	exps @ 
	(map (ExpProcess.term2exp o #name) inputs) @
	(List.mapPartial #default inputs) @
	(map (ExpProcess.term2exp o #name) outputs) @
	(Util.flatmap #contents outputs) @
	(map #condition outputs)
    end

(* returns true if any read or write uses this iterator *)
fun requiresIterator (iter: DOF.systemiterator) (class: DOF.class) =
    let	val {outputs, ...} = class
	val (iter_sym,_) = iter

	val pattern = Match.anysym_with_predlist [("TestFor:" ^ (Symbol.name iter_sym), ExpProcess.doesTermHaveIterator iter_sym)] (Symbol.symbol "a")
	fun exp_has_iterator exp = 
	    let
		val match = Match.findOnce (pattern, exp)
	    in
		Option.isSome match
	    end

	val instance_equations = List.filter ExpProcess.isInstanceEq (!(#exps class))

	val all_exps = class2exps class
    in  
	List.exists exp_has_iterator all_exps orelse
	(StdFun.listOr (map (fn(exp)=> 
			       let
				   val {classname,...} = ExpProcess.deconstructInst exp
			       in
				   requiresIterator iter (CurrentModel.classname2class classname)
			       end
			    ) instance_equations))
    end

(* just see if there are states or outputs that use this iterator... *)
fun hasStatesWithIterator (iter: DOF.systemiterator) (class: DOF.class) =
    let	val {outputs, ...} = class
	val (iter_sym, _) = iter
	(* FIXME this doesn't seem to find all the correct states; ensure read states are detected. *)
	val class_states = class2statesbyiterator iter_sym class

	val instance_equations = List.filter ExpProcess.isInstanceEq (!(#exps class))

	(*val _ = Util.log ("in class2statesizebyiterator for class '"^(Symbol.name name)^"', # of init conditions="^(i2s (List.length initial_conditions))^", # of instances=" ^ (i2s (List.length instance_equations)))*)
    in  
	List.exists (fn {name, ...} => case TermProcess.symbol2temporaliterator name
					of SOME (sym,_) => sym = iter_sym | NONE => false) (! outputs) orelse
	(List.length class_states) > 0 orelse
	(StdFun.listOr (map (fn(exp)=> 
			       let
				   val {classname,...} = ExpProcess.deconstructInst exp
			       in
				   hasStatesWithIterator iter (CurrentModel.classname2class classname)
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
		   hasStatesWithIterator iter class'
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
	(* val _ = Util.log ("Resulting flat equation: " ^ (e2s flat_equ)) *)
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
	val inputs = !(#inputs class)

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
	    let fun update_output (output as {name, contents, condition}) =
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


	fun update_output2 (output as {name, contents, condition}) =
	    let
		(* val _ = Util.log ("Processing output '"^(e2s (Exp.TERM name))^"'") *)
		(* this line will add the appropriate scope to each symbol and will add the correct temporal iterator*)
		(* TODO is this necessary? These rules have already been applied above. *)
		val contents' = map (Match.applyRewritesExp actions) contents
		val condition' = Match.applyRewritesExp actions condition

		val name = 
		    case TermProcess.symbol2temporaliterator name 
		     of NONE => 
			let val exps = map (flattenExpressionThroughInstances class) (condition' :: contents')
			    val iters = map 
					    (fn(iter_sym,_)=>iter_sym) 
					    (List.mapPartial ExpProcess.exp2temporaliterator exps)
			    (* grab the iterators used in expression (ex. y = x when t > 10) *)
			    val symbolset = SymbolSet.flatmap ExpProcess.exp2symbolset exps
			    val used_iters = List.filter (fn(iter_sym)=>SymbolSet.exists (fn(sym)=>sym=iter_sym) symbolset) indexable_iterators
			    (* val _ = Util.log ("Finding iterator for output '"^(Symbol.name (Term.sym2curname name))^"'") *)
			    (* val _ = Util.log ("Found "^(i2s (List.length iters))^" temporal iterators in " ^ (i2s (List.length exps)) ^ " expressions") *)
			    (* val _ = Util.log (String.concatWith "\n" (map ExpPrinter.exp2str exps)) *)
			in case Util.uniquify (iters @ used_iters)
			    of nil => name
			     | [iter_sym] => 
			       (case CurrentModel.itersym2iter iter_sym
				 of (_, DOF.UPDATE base_iter) => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol base_iter (Exp.TERM name))
				  | (_, DOF.POSTPROCESS base_iter) => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol base_iter (Exp.TERM name))
				  | _ => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol iter_sym (Exp.TERM name)))
			     | iters => 
			       name before
			       Logger.log_usererror nil (Printer.$("Particular output '"^(e2s (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym)=> Symbol.name sym) iters)) ^ ".  Potentially some states defining the output have incorrect iterators, or the output '"^(e2s (Exp.TERM name))^"' must have an explicit iterator defined, for example, " ^ (e2s (Exp.TERM name)) ^ "["^(Symbol.name (StdFun.hd iters))^"]."))
			end
		      |	SOME iter => name (* keep the same *)


		(* TODO: The iterator for the name should be defined in modeltranslate.  If it exists,
  		 * the iterator vector will automatically be added to the output trace.  If it doesn't exist, 
		 * only the values will be output.  This can be controlled with the "withtime" and "notime" 
		 * properties *)
		val name' = 
		    case TermProcess.symbol2temporaliterator name of
			SOME iter => name (* keep the same *)
		      | NONE => (* we have to find the iterator *)
			let
			    val sym = Term.sym2curname name
			    (* val _ = Util.log ("Finding iterator for '"^(Symbol.name sym)^"'") *)
			    val (temporal_iterators, spatial_iterators) = sym2iterators class sym
			    (* val _ = Util.log ("Found "^(i2s (List.length temporal_iterators))^" temporal and "^(i2s (List.length spatial_iterators))^" spatial") *)

			    (* transform pp[x] and update[x] into x references*)
			    fun transform_iterators (iterator, iterators) =
				let
				    val globaliterators = CurrentModel.iterators()
				    val iterator' =
					case List.find (fn(s,_) => s = iterator) globaliterators 
					 of SOME (_, DOF.POSTPROCESS it) => it
					  | SOME (_, DOF.UPDATE it) => it
					  | SOME _ => iterator
					  | NONE => DynException.stdException("No global iterator found for temporal iterator " ^ (Symbol.name iterator),
									      "ClassProcess.assignCorrectScope.outputs'",
									      Logger.INTERNAL)

				    val iterators' = if List.exists (fn(s) => s = iterator') iterators then
							 iterators
						     else
							 iterator' :: iterators
				in
				    iterators'
				end

			    val temporal_iterators' = foldl transform_iterators nil (map #1 temporal_iterators)

			    (* assign the temporal iterator first *)
			    val name' = 
				case temporal_iterators' of
				    [] => (* no iterators present, just return name *)			
				    name
				  | [iter] => ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol iter (Exp.TERM name))
				  | rest => (* this is an error *)
				    (Logger.log_usererror nil (Printer.$("Particular output '"^(e2s (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym)=> Symbol.name sym) temporal_iterators')) ^ ".  Potentially some states defining the output have incorrect iterators, or the output '"^(e2s (Exp.TERM name))^"' must have an explicit iterator defined, for example, " ^ (e2s (Exp.TERM name)) ^ "["^(Symbol.name (StdFun.hd temporal_iterators'))^"]."));
				     DynException.setErrored();
				     name)
				    
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
		{name=name', contents=contents', condition=condition'}
	    end

	val outputs = !(#outputs class)
	val outputs' = map update_output2 outputs
		      
	(* write back output changes *)
	val _ = (#outputs class) := outputs'

	(* Associates any output having no iterator dependencies to the immediate iterator. *)
	fun update_output_immediate (output as {name, contents, condition}) =
	    case (TermProcess.symbol2temporaliterator name) of
		SOME t => output
	      | NONE => 
		let 
		    val flatequ = flattenEq class (Term.sym2curname name)
		    val terms = (*Util.flatmap*) ExpProcess.exp2termsymbols (*(condition :: contents)*)flatequ
		    fun isIteratorTerm t =
			List.exists (fn(iter_sym)=> Term.sym2symname t = iter_sym) indexable_iterators
		    val name' = 
			if List.exists (fn(t)=> (Option.isSome (TermProcess.symbol2temporaliterator t)) orelse
					    (isIteratorTerm t)) terms then
			    name
			else
			    ((*Util.log("Prepending 'always' iterator to " ^ (e2s (ExpProcess.term2exp name)));*)
			     ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol (Symbol.symbol "always") (ExpProcess.term2exp name)))
		in
		    {name = name', contents = contents, condition = condition}
		end


	val _ = (#outputs class) := map update_output_immediate (! (#outputs class))
    in
	()
    end
    handle e => DynException.checkpoint "ClassProcess.assignCorrectScope" e

fun updateForkedClassScope (iter as (iter_sym, iter_type)) (class: DOF.class) =
    let
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val new_scope = Property.READSTATE (Symbol.symbol (case iter_type of 
							       DOF.UPDATE v => (Symbol.name v)
							     | _ => (Symbol.name iter_sym)))

	val pred = ("CheckingForScope",
		    case iter_type of
			DOF.UPDATE v => (fn(exp)=>
					   case exp of
					       Exp.TERM (Exp.SYMBOL (sym, props)) => 
					       (case Property.getScope props of
						    (Property.READSYSTEMSTATE v') => v = v'
						  | _ => false)
			     | _ => false)
		      | _ => (fn(exp)=>
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

(* FIXME define this algorithm more clearly. *)
fun pruneClass (iter_option, top_class) (class: DOF.class) = 
    let
	(* pull out useful quantities *)
	val name = class2orig_name class
	val inputs = !(#inputs class)
	val input_syms = map (Term.sym2symname o #name) inputs (* grab the inputs just as symbols *)
	val outputs = !(#outputs class)

	fun filter_output iterator output =
	    let val (iter_sym, _) = iterator
		val {name, contents, condition} = output
		val name' = ExpProcess.term2exp name
	    in 
		ExpProcess.doesTermHaveIterator iter_sym name' orelse
		case TermProcess.symbol2temporaliterator name 
		 of SOME (iter_sym, _) =>
		    (case CurrentModel.itersym2iter iter_sym
		      of (_, DOF.IMMEDIATE) => true
		       | _ => false)
		  | NONE => false
	    end

	val outputs' = if top_class then
			   case iter_option 
			    of SOME iter => List.filter (filter_output iter) outputs
			     | NONE => outputs
		       else
			   outputs

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
	fun inst2instname inst = #instname (ExpProcess.deconstructInst inst)
	val instance_names = map inst2instname instance_list

	(* pull only from the RHS *)
	val instance_dependencies = SymbolSet.flatmap (ExpProcess.exp2symbolset o ExpProcess.rhs) instance_list
	val dependency_list = SymbolSet.union (dependency_list, instance_dependencies)
				     

	(* do some magic here ... *)
	fun findDependencies dep_list = 
	    let
		val dep_list' = 
		    case iter_option of 
			SOME (iter_sym, _) => 
			SymbolSet.flatmap 
			    ExpProcess.exp2symbolset 
			    (Util.flatmap 
				 (symbolofoptiter2exps class iter_sym)
				 (SymbolSet.listItems dep_list))
		      | NONE => SymbolSet.flatmap ExpProcess.exp2symbolset (Util.flatmap (fn(sym)=> symbol2exps class sym) (SymbolSet.listItems dep_list))
	(*	val _ = case iter_option of
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
	fun isRequiredInstanceEq exp = ExpProcess.isInstanceEq exp andalso
				       List.exists (fn(instname)=>instname=(inst2instname exp)) instance_names
												     
	fun is_dependency exp =
	    let val symbols = ExpProcess.getLHSSymbols exp
		fun vet sym = List.exists (equal sym) symbols
	    in SymbolSet.exists vet dependency_list
	    end

	val exps' = case iter_option 
		     of SOME iter => List.filter
					 (fn (exp) => 
					    (* pull out all the exps that are state equations for that state with that iterator *)
					    ((isStateEq exp) andalso (isStateEqOfValidIterator iter exp)) orelse 
					    (* if it's not a state eq, check to see if the lhs defines what you are looking for ... *)
					    (isInitialConditionEqOfValidIterator iter exp) orelse
					    ((not (isStateEq exp)) andalso (not (isInitialConditionEq exp)) andalso is_dependency exp) orelse 
					    (* it is an instance we need with no relevent inputs/outputs *)
					    (isRequiredInstanceEq exp)

					 (*orelse
					    (not (ExpProcess.hasTemporalIterator exp))*))
					 exps
		      | NONE => List.filter is_dependency exps

	(* remove extra lhs arguments that are not on the dependency list *)
	(* this only needs to be done where the LHS is a tuple.. - this occurs only for instances right now *)
	fun test_and_replace_term t = 
	    if SymbolSet.exists 
		   (fn(sym)=>sym=(Term.sym2curname t)) 
		   dependency_list then
		t
	    else
		Exp.DONTCARE
	fun add_dontcares_to_lhs_instance exp =
	    let
		val lhs = ExpProcess.lhs exp
		val rhs = ExpProcess.rhs exp
	    in
		case lhs of
		    Exp.TERM (Exp.TUPLE terms) =>
		    let
			val terms' = map test_and_replace_term terms
			val lhs' = Exp.TERM (Exp.TUPLE terms')
		    in
			ExpBuild.equals (lhs', rhs)
		    end
		  | _ => exp
	    end				   
	val exps'' = map
			 (fn(exp)=>if ExpProcess.isInstanceEq exp then
				       add_dontcares_to_lhs_instance exp
				   else
				       exp)
			 exps'
		    
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
	((#exps class) := exps'';
	 (#outputs class) := outputs')
    end


fun optimizeClass (class: DOF.class) =
    let
	val exps = !(#exps class)

	(* first, convert all subs to adds and divs to recip *)
(*	val exps' = map (fn(exp)=> Match.applyRewritesExp [Rules.replaceSubWithNeg,
							   Rules.replaceDivWithRecip] exp) exps

	val _ = print "Round 2!\n"
	(* next, aggregate all additions *)
	val exps'' = map (fn(exp)=> Match.repeatApplyRewritesExp [Rules.distributeNeg,
								  Rules.aggregateSums1,
								  Rules.aggregateSums2,
								  Rules.aggregateProds1,
								  Rules.aggregateProds2,
								  Rules.aggregateProds] exp) exps'
*)
	val simplify = (Rules.getRules "simplification")

	val simplifyAndExpand = (Rules.getRules "simplification") @ 
				(Rules.getRules "expansion")

	val simplifyAndFactor = (Rules.getRules "simplification") @ 
				(Rules.getRules "factoring")

	val restore = (Rules.getRules "restoration")

	val exps' = map ((Match.repeatApplyRewritesExp restore) o
			 (Match.repeatApplyRewritesExp simplify))
			exps
	val exps = 
	    let
		val orig_cost = Util.sum(map Cost.exp2cost exps)
		val new_cost = Util.sum(map Cost.exp2cost exps')
	    in
		if orig_cost <= new_cost then
		    exps
		else
		    (Util.log ("  Basic simplification improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost)));
		     exps')
	    end

		       
	val exps' = map (Match.repeatApplyRewritesExp simplifyAndFactor) exps
	val exps'' = map (Match.repeatApplyRewritesExp restore) exps'

	val exps = 
	    let
		val orig_cost = Util.sum(map Cost.exp2cost exps)
		val new_cost = Util.sum(map Cost.exp2cost exps'')
	    in
		if orig_cost <= new_cost then
		    exps
		else
		    (Util.log ("  Factoring improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost)));
		     exps'')
	    end

	val exps' = map (Match.repeatApplyRewritesExp simplifyAndExpand) exps

	val exps'' = map (Match.repeatApplyRewritesExp simplifyAndFactor) exps'

	val exps''' = map (Match.repeatApplyRewritesExp restore) exps''

	val exps = 
	    let
		val orig_cost = Util.sum(map Cost.exp2cost exps)
		val new_cost = Util.sum(map Cost.exp2cost exps''')
	    in
		if orig_cost <= new_cost then
		    exps
		else
		    (Util.log ("  Expanding and factoring improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost)));
		     exps''')
	    end

	val _ = (#exps class) := exps
    in
	()
    end


fun listPairFind f (nil, _) = NONE
  | listPairFind f (_, nil) = NONE
  | listPairFind f (x::xs, y::ys) = 
    if f (x, y) then SOME (x, y) else listPairFind f (xs, ys)

(* Visits each expression, completely expanding the right-hand side of all equations. 
 * Nb dependends on a CurrentModel context. *)
fun unify class =
    let val {name, properties, inputs, outputs, iterators, exps} : DOF.class = class
    in
	{name = name, 
	 properties = properties, 
	 inputs = inputs, 
	 iterators = iterators, 
	 exps = ref (map (unifyExpression class nil) (! exps)),
	 outputs = ref (map (unifyOutput class nil) (! outputs))}
    end

(* Expands an expression. *)
and unifyExpression (class : DOF.class) blacklist exp =
    let	fun blacklisted sym =
	    List.exists (fn x => (Symbol.name x) = (Symbol.name sym)) blacklist

	val symbols = 
	    List.filter (not o blacklisted)
			(if ExpProcess.isEquation exp then
			     ExpProcess.exp2symbols (ExpProcess.rhs exp)
			 else
			     ExpProcess.exp2symbols exp)
    in
	case symbols
	 of nil => exp
	  | _ => 
	    let val _ = Util.log ("unifyExpression "^(Symbol.name (#name class))^" "^ (ExpPrinter.exp2str exp))
		val _ = Util.log ("\tsymbols " ^ (String.concatWith ", " (map Symbol.name symbols)))
		val _ = Util.log ("\tblacklist " ^ (String.concatWith ", " (map Symbol.name blacklist)))

		val expansions = List.mapPartial (symbolExpansion class blacklist) symbols

		val _ = Util.log ("unifyExpression "^(Symbol.name (#name class))^" " ^ (ExpPrinter.exp2str exp) ^ " expansions \n\t" ^ 
				  (String.concatWith "\n\t" (map (fn (sym,exp) => (Symbol.name sym) ^ ":=" ^ (ExpPrinter.exp2str exp)) expansions)))

		val rules = map (fn (find, replace) => {find = Match.asym find, replace = Rewrite.RULE replace, test = NONE}) expansions
		val exp' = Match.applyRewritesExp rules exp

		val _ = Util.log ("unifyExpression "^(Symbol.name (#name class))^" " ^ (ExpPrinter.exp2str exp) ^ " := " ^ (ExpPrinter.exp2str exp'))
	    in
		exp'
	    end
    end
    handle e => DynException.checkpoint "ClassProcess.unify.unifyExpression" e

(* Finds a symbol and maybe produces an pair of the form "(sym, expansion)". *)
and symbolExpansion (class : DOF.class) blacklist sym =
    let (* val _ = Util.log ("symbolExpansion " ^(Symbol.name (#name class))^ " " ^ (Symbol.name sym)) *)
	val expansion = 
	    case findInput class sym
	     of SOME {name, ...} 
		(* Inputs cannot be further expanded. *)
		=> NONE
	      | NONE 
		=> (case findMatchingEq class sym
		     of SOME exp 
			=> SOME (if ExpProcess.isInstanceEq exp then
				     (sym, instanceEquationExpansion class blacklist sym exp)
				 else
				     (sym, unifyExpression class (sym::blacklist) (ExpProcess.rhs exp)))
		      | NONE 
			=> DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined  in class '" ^ (Symbol.name (#name class)) ^ "'"), 
						     "ClassProcess.unify.symbolExpansion", Logger.INTERNAL))

	(* val _ = Util.log ("symbolExpansion " ^(Symbol.name (#name class))^ " " ^ (Symbol.name sym)^ " := " ^  *)
(* 			  (case expansion of SOME exp => ExpPrinter.exp2str exp | _ => "NONE")) *)
    in
	expansion
    end
	   
and instanceEquationExpansion (class : DOF.class) blacklist sym eqn =
    let val {classname, outargs, inpargs, ...} = ExpProcess.deconstructInst eqn
	val instanceClass = CurrentModel.classname2class classname
	val {outputs, ...} = instanceClass

	(* val _ = Util.log ("instanceEquationExpansion "^(Symbol.name (#name class))^" "^ (ExpPrinter.exp2str eqn)) *)

	(* Finds the output associated with this symbol. *)
	val {contents, condition, ...} =
	    case listPairFind (fn (outarg, _) => Term.sym2curname outarg = sym)
			      (outargs, ! outputs)
	     of SOME (_, output) 
		=> output
	      | NONE
		=> DynException.stdException(("Output '"^(Symbol.name sym)^"' not defined in class '" ^ (Symbol.name classname) ^ "'"), 
					     "ClassProcess.unify.instanceEquationExpansion", Logger.INTERNAL)

	val name = ExpBuild.var (Symbol.name sym)

	val value = 
	    if 1 = List.length contents then 
		List.hd contents
	    else
		DynException.stdException(("Too many quantities for output '"^(Symbol.name sym)^"' in class '" ^ (Symbol.name classname) ^ "'"), 
					  "ClassProcess.unify.instanceEquationExpansion", Logger.INTERNAL)

	(* Builds a new equation of the form "sym = if condition then contents else sym" *)
	val expansion = ExpBuild.cond (unifyExpression instanceClass (sym::blacklist) condition, 
				       unifyExpression instanceClass (sym::blacklist) value, 
				       name)
	(* val _ = Util.log ("instanceEquationExpansion "^(Symbol.name (#name class))^" := " ^ (ExpPrinter.exp2str eqn')) *)
    in
	expansion
    end

and expandIntermediateEquation (class : DOF.class) eqn = eqn

    

(* Expands the contents and condition expressions of an output. *)
and unifyOutput (class : DOF.class) blacklist output =
    let val {name, contents, condition} = output
    in
	{name = name, 
	 contents = map (unifyExpression class blacklist) contents,
	 condition = unifyExpression class blacklist condition}
    end
    handle e => DynException.checkpoint "ClassProcess.unify.unifyOutput" e



local open mlJS in
val js_symbol = js_string o Symbol.name

fun to_json (class as {name, properties, inputs, outputs, iterators, exps}) =
    let fun input_to_json {name, default} = 
	    js_object [("name", ExpProcess.term_to_json name),
		       ("default", case default of SOME exp => ExpProcess.to_json exp | NONE => js_null)]

	fun output_to_json {name, contents, condition} =
	    js_object [("name", ExpProcess.term_to_json name),
		       ("contents", js_array (map ExpProcess.to_json contents)),
		       ("condition", ExpProcess.to_json condition)]

	fun iterator_to_json {name, low, step, high} =
	    js_object [("name", js_symbol name),
		       ("low", js_float low),
		       ("step", js_float step),
		       ("high", js_float high)]

	val json_properties
	  = let val {sourcepos, basename, classform, classtype} 
		  = properties

		val json_classform
		  = case classform
		     of DOF.INSTANTIATION {readstates,writestates} =>
			js_object [("type", js_string "INSTANTIATION"),
				   ("readStates", js_array (map js_symbol readstates)),
				   ("writeStates", js_array (map js_symbol writestates))]
		      | DOF.FUNCTIONAL => js_object [("type", js_string "FUNCTIONAL")]

		val json_classtype
		  = case classtype
		     of DOF.MASTER name => js_object [("type", js_string "MASTER"), ("name", js_symbol name)]
		      | DOF.SLAVE name => js_object [("type", js_string "SLAVE"), ("name", js_symbol name)]
	    in
		js_object [("sourcePosition", PosLog.to_json sourcepos),
			   ("baseName", js_symbol basename),
			   ("classForm", json_classform),
			   ("classType", json_classtype)]
	    end
    in
	js_object [("name", js_symbol name),
		   ("properties", json_properties),
		   ("inputs", js_array (map input_to_json (!inputs))),
		   ("outputs", js_array (map output_to_json (!outputs))),
		   ("iterators", js_array (map iterator_to_json iterators)),
		   ("expressions", js_array (map ExpProcess.to_json (!exps)))]
    end
end

end
