signature CLASSPROCESS =
sig
    (* Optimization method - primary method used to call optimizations that get executed across a class. *)
    (* Note: All class optimizations work on expressions that are references, therefore most methods don't have a return value *)
    val optimizeClass : DOF.class -> unit

    (* removeRedundancy - remove common subexpressions and create additional intermediates to reduce expression cost *)
    val removeRedundancy : DOF.class -> unit

    (* Accessor methods *)    



    (* Returns a list of unique instances within the class as classname/instname pairs, mapping names to their original forms. *)
    val class2instnames : DOF.class -> (Symbol.symbol * Symbol.symbol) list 
    (* As above, but retains actual names. *)
    val class2instnames' : DOF.class -> (Symbol.symbol * Symbol.symbol) list 

    val class2orig_name : DOF.class -> Symbol.symbol (* the name of the class before it was renamed, for example during ordering *)
    val class2classname : DOF.class -> Symbol.symbol (* returns the class name as stored in the structure, not the "realclassname" *)
    val class2preshardname : DOF.class -> Symbol.symbol
    val findSymbols : DOF.class -> SymbolSet.set (* return a list of every unique symbol name used in the class - helpful for global renaming *)
    val class2states : DOF.class -> Symbol.symbol list (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    (* grab all the states in the class, determined by looking at initial conditions and/or state eq *)
    val class2statesbyiterator : Symbol.symbol -> DOF.class -> Symbol.symbol list 
    val class2statesize : DOF.class -> int (* returns the total number of states stored in the class *)
    val class2statesizebyiterator : DOF.systemiterator -> DOF.class -> int (* limit the state count to those associated with a particular temporal iterator *)
    val class2instancesbyiterator : Symbol.symbol -> DOF.class -> Exp.exp list (* grab all the instances that have states matching the specified iterator, determined by calling class2statesizebiterator *)
    val class2instances : DOF.class -> Exp.exp list 
    val symbol2exps : DOF.class -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val symbolofiter2exps :  DOF.class -> Symbol.symbol -> Symbol.symbol -> Exp.exp list (* find all expressions that match the symbol on the lhs *)
    val class2update_states : DOF.class -> Symbol.symbol list (* find all state names that have an update equation associated *)

    val inputsByIterator: DOF.systemiterator -> DOF.class -> DOF.Input.input list
    val outputsByIterator : DOF.systemiterator -> DOF.class -> DOF.Output.output list

    val outputsSymbols : DOF.class -> Exp.term list
    val outputsSymbolsByIterator : DOF.systemiterator -> DOF.class -> Exp.term list

    val findInput : DOF.class -> Symbol.symbol -> DOF.Input.input option
    val isTermInput: DOF.class -> Exp.term -> bool

    (* foldExpressions visit zero class
     * Visits each expression in a given class.
     * Analogous to List.foldl, returns the reduction of successively applying the visitor
     * function to each expression.
     *)
    val foldExpressions: 'a ExpProcess.visitor -> 'a -> DOF.class -> 'a
    val foldInputs: (DOF.Input.input * 'a -> 'a) -> 'a -> DOF.class -> 'a

    (* foldInitialValueEquations visit zero class
     * Like foldExpressions, but visits only initial value equations.
     *)
    val foldInitialValueEquations: 'a ExpProcess.visitor -> 'a -> DOF.class -> 'a
    val foldInstanceEquations: 'a ExpProcess.visitor -> 'a -> DOF.class -> 'a

    (* Indicates whether a class has a non-zero number of states. *)
    val hasStates : DOF.class -> bool
    (* Indicates whether a class has instances. *)
    val hasInstances : DOF.class -> bool
    (* Indicates whether a class contains states associated with a given iterator. *)
    val hasStatesWithIterator : DOF.systemiterator -> DOF.class -> bool
    val requiresIterator : DOF.systemiterator -> DOF.class -> bool

    (* Returns the classname associated with the MASTER/SLAVE relationship of a given class. *)
    val classTypeName: DOF.class -> Symbol.symbol


    (* Functions to modify class properties, usually recursively through the expressions *)
    val applyRewritesToClass : Rewrite.rewrite list -> DOF.class -> unit (* generic rewriting helper *)
    val applyRewritesToClassInternals : Rewrite.rewrite list -> DOF.class -> unit (* generic rewriting helper, ignore the input/output interface terms *)
    val duplicateClass : DOF.class -> Symbol.symbol -> DOF.class (* makes a duplicate class with the new supplied name *)
    val updateRealClassName : DOF.class -> Symbol.symbol -> DOF.class 
    val updatePreShardName : DOF.classproperties -> Symbol.symbol -> DOF.classproperties
    val pruneClass : (DOF.systemiterator option * bool) -> DOF.class -> unit (* prunes unneeded equations in the class, the initial bool causes all states to be kept as well *)
    val propagateSpatialIterators : DOF.class -> unit (* propagates iterators through equations into outputs *)
    val propagateStateIterators : DOF.class -> unit (* propagates just state iterators through equations into outputs *)
    val assignCorrectScope : DOF.class -> unit (* sets read state or write state properties on symbols *)
    val updateForkedClassScope : DOF.systemiterator -> DOF.class -> unit (* update the scopes on symbols for those reads that are to be read from a per-iterator state structure instead of the system state structure *)
    val addEPIndexToClass : bool -> DOF.class -> unit (* sets the embarrassingly parallel property on symbols in all but the top level class *)
    val fixSymbolNames : DOF.class -> unit (* makes all symbol names C-compliant *)

    val renameInsts :  ((Symbol.symbol * Symbol.symbol) * (Symbol.symbol * Symbol.symbol)) -> DOF.class -> unit (* change all instance names in a class *)
    val createEventIterators : DOF.class -> unit (* searches out postprocess and update iterators *)
    val addDelays : DOF.class -> unit (* adds delays to difference equation terms *)
    val addBufferedIntermediates : DOF.class -> unit (* for iterators that read and write to the same state vector, so we add intermediates to break up any loops *)
    val flattenEq : DOF.class -> Symbol.symbol -> Exp.exp (* find an equation/expression that represents the particular symbol *)
    val flattenExpressionThroughInstances : DOF.class -> Exp.exp -> Exp.exp

    (* Returns the given class with all intermediates inlined and all instance equations expanded.
     * Nb Changes the class's internal references and dependends on a CurrentModel context. *)
    val unify : DOF.class -> DOF.class
end
structure ClassProcess : CLASSPROCESS = 
struct

fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$ str)

val i2s = Util.i2s
val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr

fun equal a b = a = b

fun inputIsNamed symbol input =
    symbol = Term.sym2curname (DOF.Input.name input)

fun rewriteClass rewriter class =
    let val {inputs as ref inputs', exps as ref exps', outputs as ref outputs', ...} : DOF.class = class
	
	open DOF

	val inputs' = map ((Input.rewrite rewriter) o 
			   (Input.rename (ExpProcess.exp2term o rewriter o ExpProcess.term2exp))) inputs'
	val outputs' = map ((Output.rewrite rewriter) o 
			    (Output.rename (ExpProcess.exp2term o rewriter o ExpProcess.term2exp)) o
			    (Output.renameInputs (ExpProcess.exp2term o rewriter o ExpProcess.term2exp))) outputs'
				   

	val exps' = map rewriter exps'
    in
	inputs := inputs' before
	exps := exps' before
	outputs := outputs'
    end

(* rewriteClassInternals: only rewrite what's inside a class, don't change the interface *)
fun rewriteClassInternals rewriter class =
    let val {inputs as ref inputs', exps as ref exps', outputs as ref outputs', ...} : DOF.class = class
	
	open DOF

	val inputs' = map (Input.rewrite rewriter) inputs'
	val outputs' = map (Output.rewrite rewriter) outputs'
				   

	val exps' = map rewriter exps'
    in
	inputs := inputs' before
	exps := exps' before
	outputs := outputs'
    end

fun applyRewritesToClass rewrites (class:DOF.class) =
    rewriteClass (Match.applyRewritesExp rewrites) class
fun applyRewritesToClassInternals rewrites (class:DOF.class) =
    rewriteClassInternals (Match.applyRewritesExp rewrites) class


fun duplicateClass (class: DOF.class) new_name =
    let
	val {name, properties, inputs, outputs, exps} = class
    in
	{name=new_name,
	 properties=properties,
	 inputs=ref (!inputs),
	 outputs=ref (!outputs),
	 exps=ref (!exps)}
    end

fun updateRealClassName (class: DOF.class) new_name =
    let
	val {name, properties={sourcepos,preshardname,classform}, inputs, outputs, exps} = class
    in
	{name=name, (* this was already updated in duplicateClass *)
	 properties={sourcepos=sourcepos,
		     preshardname=preshardname,
		     classform=classform}, (* this is updated *)
	 inputs=inputs,
	 outputs=outputs,
	 exps=exps}
    end	

fun updatePreShardName (props as {sourcepos,preshardname,classform}) new_name =
    {sourcepos=sourcepos,
     preshardname=new_name,
     classform=classform}

fun class2preshardname ({properties={preshardname,...},...}:DOF.class) = preshardname

fun renameInsts (syms as ((name, new_name),(orig_name, new_orig_name))) (class: DOF.class) =
    let	
	(*val _ = Util.log("in ClassProcess.renameInsts: class=" ^ (Symbol.name (#name class)) ^ ", name="^(Symbol.name name)^" ,new_name=" ^ (Symbol.name new_name))*)
	val exps = !(#exps class)
	val outputs = !(#outputs class)

	val rename = ExpProcess.renameInst syms
	val exps' = map rename exps
	val outputs' = map (DOF.Output.rewrite rename) outputs
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
	List.exists (inputIsNamed sym) inputs
    end

fun isSymIterator sym =
    let
	val iterators = CurrentModel.iterators ()
	val indexable_iterators = List.mapPartial (fn(iter_sym, iter_type)=> case iter_type of
										 DOF.CONTINUOUS _ => SOME iter_sym
									       | DOF.DISCRETE _ => SOME iter_sym
									       | _ => NONE) iterators
    in
	List.exists (fn iter_sym => sym = iter_sym) indexable_iterators
    end			

fun isTermInput class (Exp.SYMBOL (sym,_)) = isSymInput class sym
  | isTermInput _ _ = false

fun findInput class sym =
    let val {inputs, ...} : DOF.class = class
    in List.find (inputIsNamed sym) (!inputs)
    end

fun isSymOutput (class:DOF.class) sym = 
    let
	val outputs = !(#outputs class)
    in
	List.exists (fn output => Term.sym2curname (DOF.Output.name output) = sym) outputs
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
	    (case (List.find (fn output => sym = Term.sym2curname (DOF.Output.name output)) outputs) of
		 SOME output =>
		 (case DOF.Output.contents output
		   of [] => SOME (ExpBuild.equals (Exp.TERM (DOF.Output.name output), ExpBuild.explist []))
		    | [oneexp] => SOME (ExpBuild.equals (Exp.TERM (DOF.Output.name output), oneexp))
		    | rest => SOME (ExpBuild.equals (Exp.TERM (DOF.Output.name output), ExpBuild.explist rest)))
	       | NONE => 
		 (* if there's nothing else to look at, let's check initial conditions *)
		 (case List.find 
			   (fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.exp2symbols (ExpProcess.lhs exp)))
			   init_equs of
		      SOME exp => SOME exp
		    | NONE => NONE)) (* it really doesn't exist, so throw a NONE - this might cause an exception downstream *)
		       
	     
    end

fun findStatesWithoutScope (class:DOF.class) =
    let
	val exps = !(#exps class)
	val state_equs = List.filter ExpProcess.isStateEq exps
	val statesyms = map ExpProcess.getLHSSymbol state_equs
    in
	statesyms
    end


fun isSymbolAState class sym =
    let
	val all_states = findStatesWithoutScope class
    in
	List.exists (fn(sym')=>sym=sym') all_states
    end

fun isTermAState class (Exp.SYMBOL (sym,_)) = isSymbolAState class sym
  | isTermAState class _ = false


(* match all the expressions that have that symbol on the lhs *)
fun symbol2exps (class: DOF.class) sym =
    (List.filter 
	 (fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) 
	 (!(#exps class)))
    handle e => DynException.checkpoint "ClassProcess.symbol2exps" e

fun symbolofiter2exps (class: DOF.class) iter_sym sym =
    (List.filter 
	 (fn(exp)=> List.exists (fn(sym')=>sym=sym') (ExpProcess.getLHSSymbols exp)) 
	 (List.filter (ExpProcess.doesEqHaveIterator iter_sym) (!(#exps class))))
    handle e => DynException.checkpoint "ClassProcess.symboliter2exps" e

fun symbolofoptiter2exps (class: DOF.class) iter_sym sym =
    (List.filter 
	 ((List.exists (equal sym)) o ExpProcess.getLHSSymbols)
	 (List.filter 
	      (fn (exp) => (ExpProcess.doesEqHaveIterator iter_sym exp) orelse 
			   (case ExpProcess.exp2temporaliterator exp of NONE => true | _ => false)) 
	      (!(#exps class))))
    handle e => DynException.checkpoint "ClassProcess.symboloptiter2exps" e


fun flattenExpressionThroughInstances class exp =
    if ExpProcess.isEquation exp then
	let
	    val lhs = ExpProcess.lhs exp
	    val rhs = ExpProcess.rhs exp
	    val flat_rhs = flattenExpressionThroughInstances class rhs
	in
	    ExpBuild.equals (lhs, flat_rhs)
	end
    else
	let 
	    val symbols = ExpProcess.exp2symbols exp
	    (*val _ = Util.log (" - Symbols: " ^ (Util.list2str Symbol.name symbols))*)
	    val equations = map (flattenEquationThroughInstances class) symbols
	    (*val _ = Util.log (" - Equations: " ^ (Util.list2str e2s equations))*)
	    val (intermediate_equs, other_equs) = List.partition ExpProcess.isIntermediateEq equations
	    val rules = map ExpProcess.equation2rewrite other_equs
	    val intermediate_rules = map ExpProcess.intermediateEquation2rewrite intermediate_equs
	    val exp' = Match.applyRewritesExp (rules @ intermediate_rules) exp
	(*val _ = Util.log (" - Transform '"^(e2s exp)^"' to '"^(e2s exp')^"'")*)
	in
	    exp'
	end
	handle e => DynException.checkpoint ("ClassProcess.flattenExpressionThroughInstances ["^(e2s exp)^"]") e

and flattenEquationThroughInstances class sym =
    let val {name=classname, inputs, ...} = class
	(*val _ = Util.log ("In class '"^(Symbol.name classname)^"', searching for sym '"^(Symbol.name sym)^"'")*)
    in if isSymInput class sym then
	   ExpBuild.equals (ExpBuild.var (Symbol.name sym), 
			    Exp.TERM (DOF.Input.name (valOf (List.find (inputIsNamed sym) (! inputs)))))
       else if isSymIterator sym then
	   ExpBuild.equals (ExpBuild.var (Symbol.name sym),
			    ExpBuild.var (Symbol.name sym)) (* just an identity equation t = t, which will be converted to t -> t *)
       else if isSymbolAState class sym then (* if it's a state equation *)
	   let
	       val matching_exps = symbol2exps class sym
	       val lhs_term = case List.find ExpProcess.isInitialConditionEq matching_exps of
				  SOME init_eq => ExpProcess.getLHSTerm init_eq
				| NONE => DynException.stdException (("Uncovered state '"^(Symbol.name sym)^"' without initial condition"), "ClassProcess.flattenEquationThroughInstances", Logger.INTERNAL)

	       val iter' = case TermProcess.symbol2temporaliterator lhs_term of
			       SOME (iter_sym, Iterator.ABSOLUTE 0) => (iter_sym, Iterator.RELATIVE 0)
			     | _ => DynException.stdException (("Uncovered state '"^(Symbol.name sym)^"' with unexpected iterator"), "ClassProcess.flattenEquationThroughInstances", Logger.INTERNAL)

	       val rhs = ExpProcess.updateTemporalIterator iter' (ExpProcess.term2exp lhs_term)
	   in
	       ExpBuild.equals (ExpBuild.var (Symbol.name sym),
				rhs) (* an identity equation, though the rhs has an iterator *)
	   end
       else
	   case findMatchingEq class sym
	    of SOME exp =>
	       if ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp then
		   leafTermSymbolsOfInstanceEquation class sym exp
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
			      val exp' = Match.applyRewritesExp (map make_rule locals) exp
			      (*val _ = Util.log (" -> Mapping "^(e2s exp)^" to "^(e2s exp'))*)
			  in
			      exp'
			  end
		   end
	       else 
		   exp
	     | NONE => DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.flattenEquationThroughInstances", Logger.INTERNAL)
    end
    handle e => DynException.checkpoint ("ClassProcess.flattenEquationThroughInstances ["^(Symbol.name sym)^"]") e

(* Constructs a smashed expression by associating the given symbol with an output parameter,
 * then inspecting the instance class to find the class output expression. 
 *
 * The resulting expression is the output name itself if the output symbol has an iterator defined.
 * Otherwise returned an expression "x=(...)" where the rhs is a tuple containing all leaf term symbols
 * driving the output.
 *)
and leafTermSymbolsOfInstanceEquation caller sym eqn =
    let
	val {classname, outargs, inpargs, ...} = ExpProcess.deconstructInst eqn
	val class = CurrentModel.classname2class classname
	val {outputs, inputs, ...} = class
	val output_ix = case List.find (fn (x,_) => Term.sym2curname x = sym) (Util.addCount outargs)
			 of SOME (_, index) => index
			  | NONE => 
			    DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), 
						      "ClassProcess.leafTermSymbolsOfInstanceEquation", 
						      Logger.INTERNAL)
	val output = List.nth (! outputs, output_ix)
	val (name, contents, condition) = (DOF.Output.name output, DOF.Output.contents output, DOF.Output.condition output)

	(* val _ = Util.log ("leafTermSymbolsOfInstanceEquation for sym '"^(Symbol.name sym)^"': '"^(e2s eqn)^"'") *)
    in 
	case TermProcess.symbol2temporaliterator name
	 of SOME _ => ExpBuild.equals (ExpBuild.var (Symbol.name sym), Exp.TERM name)
	  | NONE => 
	    let 
		val terms = 
		    Util.flatmap (ExpProcess.exp2termsymbols o (flattenExpressionThroughInstances class)) (condition :: contents)
		(* Outputs connected to inputs are traced back to the caller. *)
		val terms = 
		    Util.flatmap (fn t => 
				     if isSymInput class (Term.sym2curname t) then
					 let val input_ix = case List.find (fn (input, _) => inputIsNamed (Term.sym2curname t) input) (Util.addCount (! inputs))
							     of SOME (_, index) => index
							      | NONE => 
								DynException.stdException(("Symbol '"^(Symbol.name sym)^"' not defined "), "ClassProcess.leafTermSymbolsOfInstanceEquation", Logger.INTERNAL)
					     val inp = List.nth (inpargs, input_ix)
					 in 
					     ExpProcess.exp2termsymbols (flattenExpressionThroughInstances caller inp)
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
	ExpBuild.equals (ExpBuild.svar sym,
			 Exp.TERM (DOF.Input.name (valOf (List.find (inputIsNamed sym) (!(#inputs class))))))
    else if isSymIterator sym then
	ExpBuild.equals (ExpBuild.var (Symbol.name sym),
			 ExpBuild.var (Symbol.name sym))
    else
	case findMatchingEq class sym of
	    SOME exp => 
	    let
		val log = if DynamoOptions.isFlagSet "logdof" then 
			      Util.log
			  else
			   fn _ => ()
		val _ = log ("Found matching eq for sym '"^(Symbol.name sym)^"' -> '"^(e2s exp)^"'")

		val symbols = ExpProcess.exp2termsymbols (ExpProcess.rhs exp)
		val local_symbols = List.filter (not o (isTermAState class)) (List.filter Term.isLocal symbols)
		val matching_equations = map ((findMatchingEq class) o Term.sym2curname) local_symbols
		(* only use symbols that are from intermediates *)
		val filtered_symbols = map #1 
					   (List.filter 
						(fn(_,equ)=> case equ of 
								 SOME e => not (ExpProcess.isInstanceEq e orelse ExpProcess.isOutputEq e)
							       | NONE => false)
						(ListPair.zip (local_symbols,matching_equations)))


		fun term_rewrite_rule term = 
		    let
			val find = Exp.TERM term
			val test = NONE
			val _ = log ("flattenEq ("^(Symbol.name (#name class))^"): '"^(e2s find)^"'")
			val repl_exp = ExpProcess.rhs (flattenEq class (Term.sym2curname term))
			val _ = log (" ->  '"^(e2s repl_exp)^"'")
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
	val log = if DynamoOptions.isFlagSet "logdof" then 
		      Util.log
		  else
		      fn _ => ()
	val _ = log ("Flattening " ^ (e2s exp))
	val symbols = List.filter (fn sym => not ((isSymInput class sym) orelse (isSymIterator sym))) (ExpProcess.exp2symbols exp)
	val equations = map (flattenEq class) symbols
	val (intermediate_equs, other_equs) = List.partition ExpProcess.isIntermediateEq equations
	val rules = map ExpProcess.equation2rewrite other_equs
	val intermediate_rules = map ExpProcess.intermediateEquation2rewrite intermediate_equs
    in
	Match.applyRewritesExp (rules @ intermediate_rules) exp
    end
    handle e => DynException.checkpoint "ClassProcess.flattenExp" e

fun findSymbols (class: DOF.class) =
    let
	val inputs = !(#inputs class)
	val outputs = !(#outputs class)
	val exps = !(#exps class)

	fun exp2symbolset exp =
	    ExpProcess.exp2symbolset exp

	fun input2symbols input =
	    SymbolSet.fromList (ExpProcess.exp2symbols (Exp.TERM (DOF.Input.name input)) @ 
				(case DOF.Input.default input of
				     SOME v => ExpProcess.exp2symbols v
				   | NONE => []))

	fun output2symbols output =
	    SymbolSet.unionList [(exp2symbolset (Exp.TERM (DOF.Output.name output))),
				 (SymbolSet.flatmap exp2symbolset (DOF.Output.contents output)),
				 (exp2symbolset (DOF.Output.condition output))]	    

    in
	SymbolSet.unionList [SymbolSet.flatmap input2symbols inputs,
			     (SymbolSet.flatmap output2symbols outputs),
			     (SymbolSet.flatmap exp2symbolset exps)]
    end

fun findStateSymbols (class: DOF.class) =
    let
	val exps = 
	    (! (#exps class)) 
	    @ (Util.flatmap (fn output => (DOF.Output.condition output) :: (DOF.Output.contents output)) (! (#outputs class)))
	    @ (Util.flatmap (fn input => case DOF.Input.default input of SOME v => [v] | _ => nil) (! (#inputs class)))

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

	fun renameInput input =
	    DOF.Input.rewrite exp_rename (DOF.Input.rename (ExpProcess.exp2term o exp_rename o Exp.TERM) input) 
		      
	fun renameOutput output = 
	    ((DOF.Output.rewrite exp_rename) o
	     (DOF.Output.rename (ExpProcess.exp2term o exp_rename o Exp.TERM)) o
	     (DOF.Output.renameInputs (ExpProcess.exp2term o exp_rename o Exp.TERM))) output
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
	val {name,...} = class
    in
	name
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
    handle e => DynException.checkpoint "ClassProcess.class2statesbyiterator" e


fun outputsByIterator iterator (class: DOF.class) =
    let val (iter_sym, _) = iterator
	val {outputs, ...} = class

	fun has_iterator output =
	    let fun has exp =
		    case ExpProcess.exp2temporaliterator exp
		     of SOME (iter_sym', _) => iter_sym = iter_sym'
		      | _ => false
	    in has (ExpProcess.term2exp (DOF.Output.name output))
	    end
    in List.filter has_iterator (! outputs)
    end
    handle e => DynException.checkpoint "ClassProcess.outputsByIterator" e

fun inputsByIterator iterator (class: DOF.class) =
    let val (iter_sym, _) = iterator
	val {inputs, ...} = class
	fun has_iterator input =
	    let fun has exp =
		    case ExpProcess.exp2temporaliterator exp
		     of SOME (iter_sym', _) => iter_sym = iter_sym'
		      | _ => false
	    in has (ExpProcess.term2exp (DOF.Input.name input))
	    end
    in
	List.filter has_iterator (! inputs)
    end



fun outputSymbols output =
    Util.flatmap ExpProcess.exp2termsymbols ((DOF.Output.condition output) :: (DOF.Output.contents output))

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

fun 'a foldExpressions f (a: 'a) (class: DOF.class) =
    foldl f a (! (#exps class))

fun 'a foldInputs f (a: 'a) (class: DOF.class) =
    foldl f a (! (#inputs class))

fun foldInitialValueEquations f =
    foldExpressions (fn (exp, a) => if ExpProcess.isInitialConditionEq exp then f (exp, a) else a)

fun foldInstanceEquations f =
    foldExpressions (fn (exp, a) => if ExpProcess.isInstanceEq exp then f (exp, a) else a)

fun classTypeName ({name,...}: DOF.class) = name


local

    fun hasInitEq exps =
	List.exists ExpProcess.isInitialConditionEq exps

    fun hasStateEq exps =
	List.exists ExpProcess.isStateEq exps

    fun hasUpdateEq exps =
	List.exists ExpProcess.isIntermediateEq exps

    fun hasAnotherConflictingIterator exps =
	List.exists (fn(exp)=> case ExpProcess.exp2temporaliterator exp of
				   SOME (iter_sym, _) => (case CurrentModel.itersym2iter iter_sym of
							      (_, DOF.ALGEBRAIC _) => true
							    | (_, DOF.UPDATE _) => true
							    | _ => false)
				 | NONE => false) exps
in
(* return those states that update the value of a state that already has a dynamic equation *)
fun class2update_states (class : DOF.class) =
    let
	val states = class2states class

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

(* return those states that have no update, differential, or difference equations associate with them *)
fun class2constant_states (class : DOF.class) = 
    let
	val states = class2states class

	val constant_states = 
	    List.filter
		(fn(sym)=>
		   let val exps'= symbol2exps class sym
		   in (hasInitEq exps') andalso (not (hasStateEq exps')) andalso (not (hasUpdateEq exps'))
		   end)
		states
    in
	constant_states
    end


fun class2postprocess_states (class: DOF.class) = 
    let
	val iterators = CurrentModel.iterators()
	val states = class2states class
	val constant_states = class2constant_states class

	(* the only difference between a post process state and an update state is that the post process state does not have a state equation *)
	val post_process_states = 
	    List.filter
		(fn(sym)=>
		   let val exps' = symbol2exps class sym
		   in (hasInitEq exps') andalso 
		      (not (hasStateEq exps')) andalso 
		      (not (hasAnotherConflictingIterator exps')) (*andalso
		      (not (List.exists (fn(sym')=>sym=sym') constant_states))*)
		   end)
		states
    in
	post_process_states
    end
    handle e => DynException.checkpoint "ClassProcess.class2postprocess_states" e
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
				| NONE => DynException.stdException ("Unexpected lack of initial condition",
								     "ClassProcess.createEventIterators.pp_exp", 
								     Logger.INTERNAL)

		val temporal = case ExpProcess.exp2temporaliterator (init_eq) of
				   SOME v => #1 v
				 | _ => DynException.stdException ("Unexpected init condition w/o temporal iterator",
								   "ClassProcess.createEventIterators.pp_exp", 
								   Logger.INTERNAL)
		val lhs' = 
		    case lhs of 
			Exp.TERM (Exp.SYMBOL (sym, props)) => 
			if ExpProcess.isInitialConditionEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, 
						  Property.setIterator 
						      props 
						      ((Iterator.postProcessOf (Symbol.name temporal),
							Iterator.ABSOLUTE 0)::spatial)))
			else if ExpProcess.isIntermediateEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, 
						  Property.setIterator 
						      props 
						      ((Iterator.postProcessOf (Symbol.name temporal),
							Iterator.RELATIVE 1)::spatial)))
			else
			    DynException.stdException ("Unexpected non-intermediate and non-initial condition equation",
						       "ClassProcess.createEventIterators.pp_exp", 
						       Logger.INTERNAL)
		      | _ => DynException.stdException ("Non symbol on left hand side of intermediate",
							"ClassProcess.createEventIterators.pp_exp", 
							Logger.INTERNAL)
	    in
		ExpBuild.equals (lhs', ExpProcess.rhs exp)
	    end			  


	fun in_exp exp = 
	    let
		val lhs = ExpProcess.lhs exp

		val spatial = ExpProcess.exp2spatialiterators exp
		val init_eq = case List.find (ExpProcess.isInitialConditionEq) (symbol2exps class (ExpProcess.getLHSSymbol exp)) of
				  SOME eq => eq
				| NONE => DynException.stdException ("Unexpected lack of initial condition",
								     "ClassProcess.createEventIterators.pp_exp", 
								     Logger.INTERNAL)

		val temporal = case ExpProcess.exp2temporaliterator (init_eq) of
				   SOME v => #1 v
				 | _ => DynException.stdException ("Unexpected init condition w/o temporal iterator",
								   "ClassProcess.createEventIterators.pp_exp", 
								   Logger.INTERNAL)
		val lhs' = 
		    case lhs of 
			Exp.TERM (Exp.SYMBOL (sym, props)) => 
			if ExpProcess.isInitialConditionEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, 
						  Property.setIterator 
						      props 
						      ((Iterator.inProcessOf (Symbol.name temporal),
							Iterator.ABSOLUTE 0)::spatial)))
			else if ExpProcess.isIntermediateEq exp then
			    Exp.TERM (Exp.SYMBOL (sym, 
						  Property.setIterator 
						      props 
						      ((Iterator.inProcessOf (Symbol.name temporal),
							Iterator.RELATIVE 1)::spatial)))
			else
			    DynException.stdException ("Unexpected non-intermediate and non-initial condition equation",
						       "ClassProcess.createEventIterators.pp_exp", 
						       Logger.INTERNAL)
		      | _ => DynException.stdException ("Non symbol on left hand side of intermediate",
							"ClassProcess.createEventIterators.pp_exp", 
							Logger.INTERNAL)
	    in
		ExpBuild.equals (lhs', ExpProcess.rhs exp)
	    end		

	val update_states = class2update_states class

	(* this is the first time that states are analyzed in exp_process, 
	 * so there could be some user errors found *)
	val _ = DynException.checkToProceed() 
	val constant_states = class2constant_states class
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
					      in_exp exp
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
					      in_exp exp
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
				     (fn(exp)=> 
					if ExpProcess.hasTemporalIterator exp then
					    ExpProcess.updateTemporalIteratorToSymbol (iter_sym, (fn(sym)=>sym)) exp
					else
					    ExpProcess.prependIteratorToSymbol iter_sym exp))}

	val pp_rewrites = map (fn(exp)=>pp2rewrite (ExpProcess.getLHSSymbol exp, valOf (ExpProcess.exp2temporaliterator exp))) 
			      (List.filter ExpProcess.isAlgebraicStateEq exps'')

	val rewrite = Match.applyRewritesExp pp_rewrites
	val exps''' = map rewrite exps''
	val outputs' = map (DOF.Output.rewrite rewrite) outputs

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
	val states = class2states class
	fun isState sym = List.exists (fn(sym')=>sym=sym') states

	val allTerms = (Util.flatmap ExpProcess.exp2termsymbols exps) @
		       (Util.flatmap outputSymbols outputs)

	val delayedTerms = List.filter (ExpProcess.isDelayedVarDifferenceTerm o ExpProcess.term2exp) allTerms

	fun term2name_delay (t as (Exp.SYMBOL (sym, _))) = 
	    (case TermProcess.symbol2temporaliterator t of
		 SOME (iter_sym, Iterator.RELATIVE d) => {sym=sym, iter_sym=iter_sym, delay= ~d, is_state=isState sym}
	       | _ => DynException.stdException("Unexpected non-relative iterator", "ClassProcess.addDelays.term2name_delay", Logger.INTERNAL))
	  | term2name_delay _ = DynException.stdException("Unexpected non-symbol term", "ClassProcess.addDelays.term2name_delay", Logger.INTERNAL)

	val term_list = map term2name_delay delayedTerms

	val grouped_term_list : {sym: Symbol.symbol, iter_sym: Symbol.symbol, delays: int list, is_state: bool} list = (* (Symbol name * iterator symbol * list of delays) list *)
	    List.foldl (fn({sym,iter_sym,delay,is_state},l)=>
			  let
			      val (match, others) = List.partition (fn{sym=sym',...}=>sym=sym') l
			  in
			      if List.length match = 0 then
				  {sym=sym, iter_sym=iter_sym, delays=[delay], is_state=is_state}::l
			      else 
				  let
				      val e as {sym=sym',iter_sym=iter_sym',delays=d_list, is_state=is_state} = Util.hd match
				  in
				      if List.exists (fn(d')=>delay=d') d_list then
					  e::others
				      else
					  {sym=sym',iter_sym=iter_sym',delays=(delay::d_list),is_state=is_state}::others
				  end
			  end)
		       [] 
		       term_list
					   
	val exps_and_rewrites : (Exp.exp list * Rewrite.rewrite list) list = 
	    map 
	    (fn{sym,iter_sym,delays=d_list,is_state}=>
	       let
		   val max_d = StdFun.max d_list
		   val (init_condition_value, spatial_iterators) = case List.find (fn(exp)=> ExpProcess.isInitialConditionEq exp) (symbol2exps class sym) of
								       SOME exp => (ExpProcess.rhs exp, ExpProcess.exp2spatialiterators exp) 
								     | NONE => (ExpBuild.int 0,[])
									       
		   val (_, iter_type) = CurrentModel.itersym2iter iter_sym
		   val inline_iter_sym = case iter_type of
					 DOF.ALGEBRAIC (_,iter_sym') => Iterator.inProcessOf (Symbol.name iter_sym')
				       | DOF.UPDATE _ => DynException.stdException("Unexpected delay of update iterator", "ClassProcess.addDelays.exps_and_rewrites", Logger.INTERNAL)
				       | _ => Iterator.inProcessOf (Symbol.name iter_sym)

		   fun d2symname d = 
		       Symbol.symbol ("#intdelay_" ^ (Symbol.name sym) ^ "_" ^ (Util.i2s d))
		   val init_conditions = map
					     (fn(d)=>ExpBuild.equals (Exp.TERM (Exp.SYMBOL (d2symname d, (Property.setIterator Property.default_symbolproperty ((inline_iter_sym, Iterator.ABSOLUTE 0)::spatial_iterators)))), init_condition_value))
					     (List.tabulate (max_d,fn(x)=>x+1))

		   fun sym_props r = Property.setIterator Property.default_symbolproperty ((inline_iter_sym, Iterator.RELATIVE r)::spatial_iterators)
		   fun d2lhs_exp d r = Exp.TERM (Exp.SYMBOL (d2symname d, sym_props r))
		   val pp_equations = map
					  (fn(d)=>if d = 1 then 
						      ExpBuild.equals (d2lhs_exp d 1, 
								       Exp.TERM (Exp.SYMBOL (sym, 
											     if is_state then
												 Property.setIterator 
												     Property.default_symbolproperty 
												     ((iter_sym, Iterator.RELATIVE 0)::spatial_iterators)
											     else
												 Property.setIterator
												     Property.default_symbolproperty
												     spatial_iterators)))
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
	val outputs' = map (DOF.Output.rewrite (Match.applyRewritesExp rewrites)) outputs
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
	val expfilters = [ExpProcess.isUpdateEq, ExpProcess.isPreProcessStateEq, ExpProcess.isPostProcessStateEq]
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
											     "ClassProcess.addBufferedIntermediates.addBufferedIntermediatesByType", 
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
		val (intermediateEqus, writeEqs) = 
		    ListPair.unzip 
			(map
			     (fn(eqs)=>
				let
				    val lhs = ExpProcess.lhs eqs
				    val rhs = ExpProcess.rhs eqs
				    val gen_symbol = 
					case ExpProcess.getLHSSymbols eqs of
					    [sym] => "#updateintermediate_" ^ (Symbol.name sym)
					  | nil => DynException.stdException(("Unexpectedly no symbols on lhs of expression " ^ (ExpPrinter.exp2str eqs)), 
									     "ClassProcess.addBufferedIntermediates.addBufferedIntermediatesByType", Logger.INTERNAL)
					  | _ => DynException.stdException(("Can not handle a tuple on lhs of update expression " ^ (ExpPrinter.exp2str eqs)), 
									   "ClassProcess.addBufferedIntermediates.addBufferedIntermediatesByType", Logger.INTERNAL)
				in
				    (ExpBuild.equals (ExpBuild.var gen_symbol, rhs),
				     ExpBuild.equals (lhs, ExpBuild.var gen_symbol))
				end) 
			     dependentEqs)
	    in
		(#exps class) := (restEqs @ independentEqs @ intermediateEqus @ writeEqs)
	    end
    in
	app addBufferedIntermediatesByType expfilters
    end
    handle e => DynException.checkpoint "ClassBuffer.addBufferedIntermediates" e

fun addEPIndexToClass is_top (class: DOF.class) =
    let
	(* val master_class = CurrentModel.classname2class (class2classname class) *)
	val states = findStateSymbols class
	val exps = !(#exps class)
	val exps' = map (ExpProcess.enableEPIndex is_top states) exps

	val outputs = !(#outputs class)
	val outputs' = map (DOF.Output.rewrite (ExpProcess.enableEPIndex is_top states)) outputs
    in
	((#exps class) := exps';
	 (#outputs class) := outputs')
    end
    handle e => DynException.checkpoint "ClassProcess.addEPIndexToClass" e

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
		SOME {name,...} => name
	      | _ => orig_name

	(*val _ = Util.log ("In class2instname: all_classes={"^(String.concatWith ", " (map (fn(c)=> (Symbol.name (#name c) ^ ":" ^ (Symbol.name (name2orig_name (#name c))))) all_classes))^"}")*)

    in
	map (fn(c,i)=>(name2orig_name c, i)) classes_insts
    end

fun class2instnames' (class : DOF.class) : (Symbol.symbol * Symbol.symbol) list =
    let
	val inst_eqs = class2instances class
	fun classAndInstanceName eqn =
	    let val {classname, instname, ...} = ExpProcess.deconstructInst eqn
	    in 
		(classname, instname)
	    end

	fun uniq_fun ((c1,i1),(c2,i2)) = i1 = i2
    in 
	Util.uniquify_by_fun uniq_fun (map classAndInstanceName inst_eqs)
    end
	
fun class2statesize (class: DOF.class) =
    let
	val {exps(*,iterators*),...} = class
	val initial_conditions = List.filter ExpProcess.isInitialConditionEq (!exps)
	val instance_equations = List.filter ExpProcess.isInstanceEq (!exps)
    in
	Util.sum ((map ExpProcess.exp2size initial_conditions) @
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
	(map (ExpProcess.term2exp o DOF.Input.name) inputs) @
	(List.mapPartial DOF.Input.default inputs) @
	(map (ExpProcess.term2exp o DOF.Output.name) outputs) @
	(Util.flatmap DOF.Output.contents outputs) @
	(map DOF.Output.condition outputs)
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
	List.exists (fn output => case TermProcess.symbol2temporaliterator (DOF.Output.name output)
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
	val {name,exps,(*iterators,*)...} = class
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
	Util.sum ((map ExpProcess.exp2size initial_conditions) @
		  (map (fn(exp)=> 
			  let
			      val {classname,...} = ExpProcess.deconstructInst exp
			  in
			      class2statesizebyiterator iter (CurrentModel.classname2class classname)
			  end
		       ) instance_equations))
    end
    handle e => DynException.checkpoint "ClassProcess.class2statesizebyiterator" e


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
	
(* this will propagate an iterator from an input to an output *)
fun propagateSpatialIterators (class: DOF.class) =
    let
	val assigned_symbols = (map (Exp.TERM o DOF.Input.name) (!(#inputs class))) @
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
		    
	val outputs' = map ((DOF.Output.rewrite (Match.applyRewritesExp rewrites)) o 
			    (DOF.Output.rename (ExpProcess.exp2term o (Match.applyRewritesExp rewrites) o Exp.TERM)))
		       (!(#outputs class))
    in
	(#exps class := exps';
	 #outputs class := outputs')
    end

(* takes a symbol name, finds an equation and returns all the iterators *)
fun sym2iterators (class: DOF.class) sym =
    let
	val flat_equ = (*flattenEq*)flattenEquationThroughInstances class sym
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

fun propagateStateIterators (class: DOF.class) =
    let
	val exps = !(#exps class)

	val state_equations = List.filter ExpProcess.isStateEq exps
	val state_terms = map ExpProcess.lhs state_equations
	val symbols = map (Term.sym2curname o ExpProcess.exp2term) state_terms
	val state_iterators_options = map (TermProcess.symbol2temporaliterator o ExpProcess.exp2term) state_terms
	val iterators = CurrentModel.iterators()

	val symbol_state_iterators = 
	    let
		val all_state_iterators = 
		    map (fn(exp, iter)=>
			   case iter of
			       SOME i => i
			     | NONE => DynException.stdException(("State '"^(e2s exp)^"' does not have temporal iterator associated with it"), "ClassProcess.propagateStateIterators", Logger.INTERNAL))
			(ListPair.zip (state_terms, state_iterators_options))

		(* we have to prune update iterators *)
		fun isUpdateIterator (iter_sym, _) =
		    List.exists (fn(iter_sym', iter_type)=> case iter_type of
								DOF.UPDATE _ => iter_sym = iter_sym'
							      | _ => false) iterators
	    in
		List.filter (fn(_, iter)=> not (isUpdateIterator iter)) (ListPair.zip (symbols, all_state_iterators))
	    end

	val actions = map 
			  (fn(sym, iter as (itersym, _))=>
			     {find=Match.asym sym, 
			      test=NONE, 
			      replace=Rewrite.ACTION (Symbol.symbol (Symbol.name sym ^ "["^(Symbol.name (#1 iter))^"]"),
						      (fn(exp)=> ExpProcess.prependIteratorToSymbol itersym exp))}) 
			  symbol_state_iterators

	val exps' = map (fn(exp) => Match.applyRewritesExp actions exp) exps


	val outputs' = map ((DOF.Output.rewrite (Match.applyRewritesExp actions)) o 
			    (DOF.Output.rename (ExpProcess.exp2term o (Match.applyRewritesExp actions) o Exp.TERM)))
		       (!(#outputs class))

	val _ = (#exps class) := exps'
	val _ = (#outputs class) := outputs'

    in
	()
    end

fun assignCorrectScope (class: DOF.class) =
    let
	val exps = !(#exps class)
	val inputs = !(#inputs class)

	val state_equations = List.filter (*ExpProcess.isStateEq*) ExpProcess.isInitialConditionEq exps
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
			     | NONE => DynException.stdException
					   ("State '"^(e2s exp)^"' does not have temporal iterator associated with it",
					    "ClassProcess.assignCorrectScope", 
					    Logger.INTERNAL))
			(ListPair.zip (state_terms, state_iterators_options))

		(* we have to prune update iterators *)
		fun isUpdateIterator (iter_sym, _) =
		    List.exists (fn(iter_sym', iter_type)=> case iter_type of
								DOF.UPDATE _ => iter_sym = iter_sym'
							      | _ => false) iterators
	    in
		List.filter (fn(_, iter)=> not (isUpdateIterator iter)) (ListPair.zip (symbols, all_state_iterators))
	    end


	(* val _ = Util.log ("In assignCorrectScope, producing rewriting rules: ") *)
	(* val _ = app (fn(sym, (iter_sym,_))=> Util.log (" -> sym: " ^ (Symbol.name sym) ^ ", iter: " ^ (Symbol.name iter_sym))) symbol_state_iterators *)
		

	val state_actions = map 
			  (fn(sym, iter as (itersym, _))=>
			     {find=Match.asym sym, 
			      test=NONE, 
			      replace=Rewrite.ACTION (Symbol.symbol (Symbol.name sym ^ "["^(Symbol.name itersym)^"]"),
						      (fn(exp)=>ExpProcess.assignCorrectScopeOnSymbol
								    (ExpProcess.prependIteratorToSymbol itersym exp)))}) 
			  symbol_state_iterators

	(* Any symbol representing the value of an iterator must be scoped accordingly.
	 * All iterator symbols are scoped via ITERATOR here.
	 * When the model is sharded later, non-local iterators will be scoped via SYSTEMITERATOR. *)
	fun makeIteratorRule sym =
	    let 
		fun action (Exp.TERM (Exp.SYMBOL (s, p))) =
		    Exp.TERM (Exp.SYMBOL (s, Property.setScope p Property.ITERATOR))
		  | action _ = DynException.stdException
				   ("Unexpected expression",
				    "ClassProcess.assignCorrectScope.iter_actions", 
				    Logger.INTERNAL)
	    in
		{find = Match.asym sym, test = NONE, replace = Rewrite.ACTION (sym, action)}
	    end

	val iter_actions = map makeIteratorRule indexable_iterators


	val actions = state_actions @ iter_actions

	val exps' = map (fn(exp) => Match.applyRewritesExp actions exp) exps


	val outputs' = map ((DOF.Output.rewrite (Match.applyRewritesExp actions)) o 
			    (DOF.Output.rename (ExpProcess.exp2term o (Match.applyRewritesExp actions) o Exp.TERM)))
		       (!(#outputs class))


	(* write back expression changes *)
	val _ = (#exps class) := exps'
	val _ = (#outputs class) := outputs'


	fun update_output2 (output) =
	    let
		val (name, contents, condition) = (DOF.Output.name output, DOF.Output.contents output, DOF.Output.condition output)
		(* val _ = Util.log ("Processing output '"^(e2s (Exp.TERM name))^"'") *)
		(* this line will add the appropriate scope to each symbol and will add the correct temporal iterator*)
		(* TODO is this necessary? These rules have already been applied above. *)
		val contents' = map (Match.applyRewritesExp actions) contents
		val condition' = Match.applyRewritesExp actions condition

		val name = 
		    case TermProcess.symbol2temporaliterator name 
		     of NONE => 
			let 
			    val exps = map (flattenExpressionThroughInstances class) (condition' :: contents')

			    (* Flattening an output expressions may result in a TUPLE
			     * if the output is grouped. Further flatten any TUPLE into
			     * its component terms. *)
			    fun flattenTuples (Exp.TERM (Exp.TUPLE terms)) = map Exp.TERM terms
			      | flattenTuples exp = [exp]

			    val exps = Util.flatmap flattenTuples exps

				       
			    val iters = SymbolSet.listItems (foldl SymbolSet.union SymbolSet.empty (map ExpProcess.iterators_of_expression exps))
			    (* grab the iterators used in expression (ex. y = x when t > 10) *)

			    (* Need to grab all iterators with the iterator tag *)
			    val symbolset = SymbolSet.flatmap ExpProcess.exp2symbolset exps
			    val used_iters = List.filter (fn(iter_sym)=>SymbolSet.exists (fn(sym)=>sym=iter_sym) symbolset) indexable_iterators
			    (* val _ = Util.log ("Finding iterator for output '"^(Symbol.name (Term.sym2curname name))^"'") *)
			    (* val _ = Util.log ("Found "^(i2s (List.length iters))^" temporal iterators in " ^ (i2s (List.length exps)) ^ " expressions:") *)
			    (* val _ = Util.log ("\t"^(String.concatWith "\n\t" (map ExpPrinter.exp2prettystr exps))) *)
			    (* val _ = Util.log (String.concatWith "\n" (map ExpPrinter.exp2str exps)) *)

			    fun iter2baseiter iter_sym = 
				let
				    val (_, iter_type) = CurrentModel.itersym2iter iter_sym
				in
				    case iter_type of
					DOF.UPDATE base_iter => base_iter
				      | DOF.ALGEBRAIC (_, base_iter) => base_iter
				      | _ => iter_sym
				end

			in case Util.uniquify (map iter2baseiter (iters @ used_iters))
			    of nil => name
			     | [iter_sym] => 
			       (case CurrentModel.itersym2iter iter_sym
				 of (_, DOF.UPDATE base_iter) => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol base_iter (Exp.TERM name))
				  | (_, DOF.ALGEBRAIC (_,base_iter)) => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol base_iter (Exp.TERM name))
				  | _ => 
				    ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol iter_sym (Exp.TERM name)))
			     | iters => 
			       name before
			       (Logger.log_error (Printer.$("Particular output '"^(e2ps (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym)=> Symbol.name sym) iters)) ^ ".  Potentially some states defining the output have incorrect iterators, or the output '"^(e2ps (Exp.TERM name))^"' must have an explicit iterator defined, for example, " ^ (e2ps (Exp.TERM name)) ^ "["^(Symbol.name (StdFun.hd iters))^"]."));
				DynException.setErrored())
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
					 of SOME (_, DOF.ALGEBRAIC (_,it)) => it
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
				    (Logger.log_error (Printer.$("Particular output '"^(e2ps (Exp.TERM name))^"' has more than one temporal iterator driving the value.  Iterators are: " ^ (Util.l2s (map (fn(sym)=> Symbol.name sym) temporal_iterators')) ^ ".  Potentially some states defining the output have incorrect iterators, or the output '"^(e2ps (Exp.TERM name))^"' must have an explicit iterator defined, for example, " ^ (e2ps (Exp.TERM name)) ^ "["^(Symbol.name (StdFun.hd temporal_iterators'))^"]."));
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
		val inputs' = ref (! (DOF.Output.inputs output))
	    in
		(* FIXME turn this into a call to DOF.Output.rewrite and DOF.Output.rename *)
		DOF.Output.make {name=name', inputs=inputs', contents=contents', condition=condition'}
	    end
	    handle e => DynException.checkpoint ("ClassProcess.AssignCorrectScope.update_output2 [name="^(e2s (Exp.TERM (DOF.Output.name output)))^"]") e

	val outputs = !(#outputs class)
	val outputs' = map update_output2 outputs
		      
	(* write back output changes *)
	val _ = (#outputs class) := outputs'

	(* Associates any output having no iterator dependencies to the immediate iterator. *)
	fun update_output_immediate output =
	    let
 		val name = DOF.Output.name output
	    in
		case (TermProcess.symbol2temporaliterator name) of
		    SOME t => output
		  | NONE => 
		    let 
			val flatequ = flattenEquationThroughInstances class (Term.sym2curname name)
			val terms = ExpProcess.exp2termsymbols flatequ
			fun isIteratorTerm t =
			    List.exists (fn(iter_sym)=> Term.sym2symname t = iter_sym) indexable_iterators
			val name' = 
			    if List.exists (fn(t)=> (Option.isSome (TermProcess.symbol2temporaliterator t)) orelse
						    (isIteratorTerm t)) terms then
				name
			    else
				((*Util.log("Prepending 'always' iterator to " ^ (e2s (ExpProcess.term2exp name)));
				  Util.log(" -> FlatEqu: " ^ (e2s flatequ));*)
				 ExpProcess.exp2term (ExpProcess.prependIteratorToSymbol (Symbol.symbol "always") (ExpProcess.term2exp name)))
		    in
			DOF.Output.rename (fn _ => name') output
		    end
	    end


	val _ = (#outputs class) := map update_output_immediate (! (#outputs class))
    in
	()
    end
    handle e => DynException.checkpoint ("ClassProcess.assignCorrectScope [class="^(Symbol.name (#name class))^"]") e

fun updateForkedClassScope (iter as (iter_sym, iter_type)) (class: DOF.class) =
    let
	val exps = !(#exps class)
	val outputs = !(#outputs class)


	val state_equations = List.filter ExpProcess.isStateEq exps
	val state_terms = map ExpProcess.lhs state_equations
	val symbols = map (Term.sym2curname o ExpProcess.exp2term) state_terms
	val state_iterators_options = map (TermProcess.symbol2temporaliterator o ExpProcess.exp2term) state_terms
	val iterators = CurrentModel.iterators()
	val indexable_iterators = List.mapPartial (fn(iter_sym, iter_type)=> case iter_type of
										 DOF.CONTINUOUS _ => SOME iter_sym
									       | DOF.DISCRETE _ => SOME iter_sym
									       | _ => NONE) iterators

	(* Finds any non-local iterators and scopes them via SYSTEMITERATOR. *)
	val systemIteratorRule =
	    let
		fun isIndexableIteratorSymbol (Exp.TERM (Exp.SYMBOL (sym, _)))  =
		    (case List.find (fn iter_sym => iter_sym = sym) indexable_iterators
		      of SOME _ => true | NONE => false)
		  | isIndexableIteratorSymbol _ = false

		fun isMyIteratorSymbol (Exp.TERM (Exp.SYMBOL (sym, _))) =
		    (case iter_type
		      of DOF.UPDATE base => base = sym
		       | DOF.ALGEBRAIC (_, base) => base = sym
		       | _ => sym = iter_sym)
		  | isMyIteratorSymbol _ = false

		fun predicate (exp, _) = 
		    isIndexableIteratorSymbol exp andalso not (isMyIteratorSymbol exp)
							  
		fun action (Exp.TERM (Exp.SYMBOL (sym, props))) =
		    let 
			val props' = Property.setScope props Property.SYSTEMITERATOR
			val props' = Property.setEPIndex props' (SOME Property.ARRAY)
		    in
			Exp.TERM (Exp.SYMBOL (sym, props'))
		    end
		  | action _ = 
		    DynException.stdException
			("Unexpected expression",
			 "ClassProcess.assignCorrectScope.iter_actions", 
			 Logger.INTERNAL)
	    in
		{find = Match.onesym "an iterator?",
		 test = SOME predicate,
		 replace = Rewrite.ACTION (iter_sym, action)}
	    end



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

	val rule = {find=find, test=NONE, 
		    replace=Rewrite.ACTION (Symbol.symbol ("UpdateForkedClassIterTo:"^(Symbol.name iter_sym)), 
					    action)}


	val rewrite = Match.applyRewritesExp [rule, systemIteratorRule]

	val exps' = map rewrite exps

	val outputs' = map (DOF.Output.rewrite rewrite) outputs
    in
	( #exps class := exps'
	; #outputs class := outputs')
    end

(* Removes unused input terms. *)
fun pruneInputsFromClass top_class class =
    let
	val inputs = #inputs class
	val exps = #exps class
	val outputs = #outputs class
	val exps' = 
	    (if top_class then
		 Util.flatmap (fn output => (DOF.Output.condition output) :: (DOF.Output.contents output)) (! outputs)
	     else 
		 nil) @ 
	    (! exps)

	val inputSymbols = map (Term.sym2curname o DOF.Input.name) (! inputs)
	val inputSymbols' =
	    List.filter
		(fn sym => List.exists (Match.exists (Match.asym sym)) exps') inputSymbols

	fun is_input_symbol sym =
	    List.exists (fn sym' => sym = sym') inputSymbols'
		
	val inputs' =
	    List.filter (is_input_symbol o Term.sym2curname o DOF.Input.name) (! inputs)
    in
	inputs := inputs'
    end

fun pruneInputsFromOutput class output =
    let
	val inputs = DOF.Output.inputs output
	val inputSymbols = map Term.sym2curname (! inputs)
	val exps = map (flattenExp class) 
			((DOF.Output.condition output) ::
			 (DOF.Output.contents output))

	val inputSymbols' = 
	    List.filter
	    (fn sym => 
		let 
		    val pattern = Match.asym sym
		in
		    List.exists (Match.exists pattern) exps
		end) inputSymbols
    in
	inputs := map (ExpProcess.exp2term o ExpBuild.svar) inputSymbols'
    end

(* 
 * pruneClass - prunes equations in the class that are not needed, modifies class in memory
 * 
 * input args:
 *   iter_option: (DOF.systemiterator option) - if specified, only maintains those outputs/states with the matching iterator, otherwise all outputs/states are maintained
 *   top_class: bool - redundant, and needs to be removed
 *   class: DOF.class - input class
 *
 * output args: none
 *)
fun pruneClass (iter_option, top_class) (class: DOF.class) = 
    let
	val log = if DynamoOptions.isFlagSet "logdof" then 
		      Util.log
		  else
		      fn _ => ()

	(* pull out useful quantities *)
	val name = class2orig_name class
	val inputs = !(#inputs class)
	val input_syms = map (Term.sym2symname o DOF.Input.name) inputs (* grab the inputs just as symbols *)
	val outputs = !(#outputs class)

	val _ = pruneInputsFromClass top_class class

	fun filter_output iterator output =
	    let val (iter_sym, iter_type) = iterator
	    in 
		ExpProcess.doesTermHaveIterator iter_sym (ExpProcess.term2exp (DOF.Output.name output))
	    (* orelse
		case TermProcess.symbol2temporaliterator name 
		 of SOME (iter_sym, _) =>
		    (case CurrentModel.itersym2iter iter_sym
		      of (_, DOF.IMMEDIATE) => true
		       | _ => false)
		  | NONE => false*)
	    end

	val outputs' = if top_class then
			   case iter_option 
			    of SOME iter => List.filter (filter_output iter) outputs
			     | NONE => outputs
		       else
			   outputs

	val _ = app (pruneInputsFromOutput class) outputs'

	val output_symbols = Util.flatmap ExpProcess.exp2symbols (map DOF.Output.condition outputs' @ 
								  (Util.flatmap DOF.Output.contents outputs'))
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
			 (fn(exp)=>if ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp then
				       (log ("Removing unused outputs of " ^ (e2s exp))
				      ; add_dontcares_to_lhs_instance exp)
				   else
				       exp)
			 exps'

	fun remove_unused_inputs (Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE outargs), Exp.FUN (func as Fun.OUTPUT {classname, instname, outname, props}, inpargs)])) =
	    let
		val class' = CurrentModel.classname2class classname

		val output = 
		    case List.find (fn out => outname = Term.sym2symname (DOF.Output.name out)) (!(#outputs class'))
		     of SOME x => x
		      | NONE => 
			DynException.stdException(("Could not find output named " ^ (Symbol.name outname) ^ " in class " ^ (Symbol.name classname) ^ "."),
						  "ClassProcess.pruneClass.remove_unused_inputs",
						  Logger.INTERNAL)

		val inputSymbols = map Term.sym2curname (!(DOF.Output.inputs output))

		val inpassoc = 
		    case inpargs
		     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
		      | _ =>
			DynException.stdException(("Inputs of output call should be an ASSOC container."),
						  "CParallelWriter.class_flow_code.instaceeq2prog",
						  Logger.INTERNAL)
			
		fun is_input_symbol sym =
		    List.exists (fn sym' => sym = sym') inputSymbols
		    
		val inpargs' =
		    Exp.CONTAINER 
			(Exp.ASSOC
			     (SymbolTable.filteri (fn (k,v) => is_input_symbol k) inpassoc))
		val rhs = Exp.FUN (func, [inpargs'])
	    in
		ExpBuild.equals (Exp.TERM (Exp.TUPLE outargs), rhs)
	    end

	  | remove_unused_inputs (Exp.FUN (Fun.BUILTIN Fun.ASSIGN, [Exp.TERM (Exp.TUPLE outargs), Exp.FUN (func as Fun.INST {classname, instname, props}, inpargs)])) =
	    let
		val class' = CurrentModel.classname2class classname

		val inputSymbols = map (Term.sym2symname o DOF.Input.name) (!(#inputs class'))

		val inpassoc = 
		    case inpargs
		     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
		      | _ =>
			DynException.stdException(("Inputs of output call should be an ASSOC container."),
						  "CParallelWriter.class_flow_code.instaceeq2prog",
						  Logger.INTERNAL)
			
		fun is_input_symbol sym =
		    List.exists (fn sym' => sym = sym') inputSymbols
		    
		val inpargs' =
		    Exp.CONTAINER 
			(Exp.ASSOC
			     (SymbolTable.filteri (fn (k,v) => is_input_symbol k) inpassoc))

		val rhs = Exp.FUN (func, [inpargs'])
	    in
		ExpBuild.equals (Exp.TERM (Exp.TUPLE outargs), rhs)
	    end

	  | remove_unused_inputs _ =
	    DynException.stdException(("Malformed equation."),
				      "ClassProcess.pruneClass.remove_unused_inputs",
				      Logger.INTERNAL)


	val exps'' = 
	    map (fn exp => if ExpProcess.isInstanceEq exp orelse ExpProcess.isOutputEq exp then
			       (log ("Removing unused inputs of " ^ (e2s exp))
			      ; remove_unused_inputs exp)
			   else
			       exp) exps''

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
		    (Logger.log_notice (Printer.$ ("  Basic simplification improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost))));
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
		    (Logger.log_notice (Printer.$ ("  Factoring improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost))));
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
		    (Logger.log_notice (Printer.$ ("  Expanding and factoring improved class " ^ (Symbol.name (#name class)) ^ " from " ^ (Int.toString (orig_cost)) ^ " to " ^ (Int.toString (new_cost))));
		     exps''')
	    end

	val _ = (#exps class) := exps
    in
	()
    end

fun unify class = 
    (inlineIntermediates o expandInstances) class

and inlineIntermediates class =
    let val {name, properties, inputs, outputs, exps} : DOF.class = class
        (* Extracts all intermediate equations from the class's member expressions.
         * Each remaining expression is rewritten by replacing any symbol referencing
	 * an intermediate computation with the expression representing that computation.
	 * The same inlining rewrite is performed on the class's output contents and conditions. 
	 * Does not recur through instance equations.
	 *)
	val exps' = (! exps)

	val (intermediateEquations, exps') = 
	    List.partition ExpProcess.isIntermediateEq exps'

	val _ = exps := exps'

        val rewrites = map ExpProcess.intermediateEquation2rewrite intermediateEquations

	(* FIXME this has N^2 complexity in matching each intermediate against itself and every other. *)
	(* First inlines intermediates within the right-hand expression of the intermediates themselves. *)
	val intermediateEquations' = 
	    map (fn eqn =>
                 let val lhs = ExpProcess.lhs eqn
                     val rhs = ExpProcess.rhs eqn
              	     val rhs' = Match.repeatApplyRewritesExp rewrites rhs
                 in
                     ExpBuild.equals (lhs, rhs')
                 end) intermediateEquations

	(* Then recomputes the rules before applying them to the remainder of the class. *)
	val rewrites = map ExpProcess.intermediateEquation2rewrite intermediateEquations'

	val _ = rewriteClassInternals (Match.repeatApplyRewritesExp rewrites) class
    in
        class
    end

and expandInstances class =
    let val {name, properties, inputs, outputs, exps} : DOF.class = class
        (* Detects instance equations within a class's member expressions.
	 * Finds the class definition  for the instance via CurrentModel and
	 * expands the instance into a list of equations.
	 * Constructs expressions to assign inputs and outputs.
	 * Renames all symbols by prefixing the instance name. *)
        val exps' = ! exps

        val _ = exps := Util.flatmap (fn exp => if ExpProcess.isInstanceEq exp then 
						    instanceExpressions exp 
						else if ExpProcess.isOutputEq exp then
						    outputExpressions exp
						else 
						    [exp]) 
				     exps'
    in
        class
    end

(* Nb (look up the latin: thats "nota bene," or literally, "note well.") depends on a CurrentModel context. *)
and outputExpressions equation =
    let 
	val {instname, classname, outargs, inpargs, ...} = ExpProcess.deconstructInst equation
	val instanceClass = CurrentModel.classname2class classname
	val instanceName = ExpProcess.instOrigInstName equation
	val {name, exps, outputs, inputs, ...} : DOF.class = instanceClass
	fun prefixSymbol prefix (Exp.TERM (Exp.SYMBOL (sym, props))) =
	    Exp.TERM (Exp.SYMBOL (Symbol.symbol (prefix ^ (Symbol.name sym)), case Property.getRealName props of 
										  SOME v => Property.setRealName props (Symbol.symbol (prefix ^ (Symbol.name v)))
										| NONE => props))

	  | prefixSymbol _ _ = 
	    DynException.stdException(("Cannot rename non-symbol in instance '" ^ (Symbol.name instname) ^ "' of class '" ^ (Symbol.name name) ^ "'"), 
				      "ClassProcess.unify.symbolExpansion", Logger.INTERNAL)
	    
	fun renameWithPrefix pref =
	    {find = Match.anysym_with_predlist [("IS_SYMBOL", ExpProcess.isSymbol),
						("NOT_ITERATOR", not o ExpProcess.isIterator)] (Symbol.symbol "anysym"),
	     replace = Rewrite.ACTION (Symbol.symbol ("renameWithPrefix:"^(Symbol.name pref)), 
				       prefixSymbol ((Symbol.name pref) ^ "#_")),
	     test = NONE}

	val renameWithInstanceNamePrefix = renameWithPrefix (ExpProcess.instOrigInstName equation)

	val inpassoc = 
	    case inpargs
	     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
	      | _ =>
		DynException.stdException(("Inputs of output call should be an ASSOC container."),
					  "CParallelWriter.class_flow_code.instaceeq2prog",
					  Logger.INTERNAL)

	val inputs_exps =
	    SymbolTable.foldli
		(fn (k,v,acc) =>
		    let 
			val name = prefixSymbol ((Symbol.name instanceName) ^ "#_") (ExpBuild.svar k)
		    in
			(ExpBuild.equals (name, v)) :: acc
		    end) 
		nil inpassoc

	fun makeOutputExpression (outarg, output) =
	    let open DOF
		val (name, contents, condition) = (Output.name output, Output.contents output, Output.condition output)
		val value = 
		    if 1 = List.length contents then List.hd contents
		    else DynException.stdException(("Too many quantities for output '"^(Symbol.name (Term.sym2curname name))^"' in class '" ^ (Symbol.name classname) ^ "'"), 
						   "ClassProcess.unify.instanceEquationExpansion", Logger.INTERNAL)
		val condition' = Match.applyRewriteExp renameWithInstanceNamePrefix condition
		val value' = Match.applyRewriteExp renameWithInstanceNamePrefix value
		val name' = Exp.TERM outarg

		val output' = 
		    case condition'
                      of Exp.TERM (Exp.BOOL true) => value'
                       | _ => ExpBuild.cond (condition', value', name')
	    in
		ExpBuild.equals (name', output')
	    end

	val outputs_exps = 
	    ListPair.map makeOutputExpression (outargs, ! outputs)
    in
	inputs_exps @
	outputs_exps
    end
    
and instanceExpressions equation =
    let val {instname, classname, outargs, inpargs, ...} = ExpProcess.deconstructInst equation
	val instanceClass = CurrentModel.classname2class classname
	val instanceName = ExpProcess.instOrigInstName equation
	val {name, exps, outputs, inputs, ...} : DOF.class = instanceClass
	val exps' = ! exps

	(* Recursively expands any other instances within the expressions of this instance's class. *)
	val exps' = Util.flatmap (fn exp => if ExpProcess.isInstanceEq exp then
						instanceExpressions exp
					    else if ExpProcess.isOutputEq exp then
						outputExpressions exp
					    else [exp]) 
				 exps'

	fun prefixSymbol prefix (Exp.TERM (Exp.SYMBOL (sym, props))) =
	    Exp.TERM (Exp.SYMBOL (Symbol.symbol (prefix ^ (Symbol.name sym)), case Property.getRealName props of 
										  SOME v => Property.setRealName props (Symbol.symbol (prefix ^ (Symbol.name v)))
										| NONE => props))

	  | prefixSymbol _ _ = 
	    DynException.stdException(("Cannot rename non-symbol in instance '" ^ (Symbol.name instname) ^ "' of class '" ^ (Symbol.name name) ^ "'"), 
				      "ClassProcess.unify.symbolExpansion", Logger.INTERNAL)
	    
	fun renameWithPrefix pref =
	    {find = Match.anysym_with_predlist [("IS_SYMBOL", ExpProcess.isSymbol),
						("NOT_ITERATOR", not o ExpProcess.isIterator)] (Symbol.symbol "anysym"),
	     replace = Rewrite.ACTION (Symbol.symbol ("renameWithPrefix:"^(Symbol.name pref)), 
				       prefixSymbol ((Symbol.name pref) ^ "#_")),
	     test = NONE}

	val renameWithInstanceNamePrefix = renameWithPrefix instanceName

	val inpassoc = 
	    case inpargs
	     of [Exp.CONTAINER (Exp.ASSOC tab)] => tab
	      | _ =>
		DynException.stdException(("Inputs of output call should be an ASSOC container."),
					  "CParallelWriter.class_flow_code.instaceeq2prog",
					  Logger.INTERNAL)

	val inputs_exps =
	    SymbolTable.foldli
		(fn (k,v,acc) =>
		    let 
			val name = prefixSymbol ((Symbol.name instanceName) ^ "#_") (ExpBuild.svar k)
		    in
			(ExpBuild.equals (name, v)) :: acc
		    end) 
		nil inpassoc

	val exps' = map (Match.applyRewriteExp renameWithInstanceNamePrefix) exps'

	fun makeOutputExpression (outarg, output) =
	    let open DOF
		val (name, contents, condition) = (Output.name output, Output.contents output, Output.condition output)
		val value = 
		    if 1 = List.length contents then List.hd contents
		    else DynException.stdException(("Too many quantities for output '"^(Symbol.name (Term.sym2curname name))^"' in class '" ^ (Symbol.name classname) ^ "'"), 
						   "ClassProcess.unify.instanceEquationExpansion", Logger.INTERNAL)
		val condition' = Match.applyRewriteExp renameWithInstanceNamePrefix condition
		val value' = Match.applyRewriteExp renameWithInstanceNamePrefix value
		val name' = Exp.TERM outarg

		val output' = 
		    case condition'
                      of Exp.TERM (Exp.BOOL true) => value'
                       | _ => ExpBuild.cond (condition', value', name')
	    in
		ExpBuild.equals (name', output')
	    end

	val outputs_exps = 
	    ListPair.map makeOutputExpression (outargs, ! outputs)
    in
	inputs_exps @
	exps' @
	outputs_exps
    end

(* removeRedundancy - remove extra dedundancy in expressions *)
local
    fun groupSubExpressions subexps =
	let
	    (* existsIn - return an optional index to of where an expression is in the list *)
	    fun existsIn (e, l) = 
		case List.find (fn((e',_),_)=> ExpEquality.equiv (e, e')) (Util.addCount l) of
		     SOME (_, n) => SOME n
		   | NONE => NONE

	    (* increment_matches - adds one to the list indicating that it was found *)
	    fun increment_matches (l, i) =
		    (Util.take (l, i)) @ [inc(Util.nth (l, i))] @ (Util.drop (l, i+1))
	    and inc (e, n) = (e, n+1)
		
	in
	    foldl
		(fn(exp, matched_list)=> case existsIn(exp, matched_list) of
					     SOME index => increment_matches (matched_list, index)
					   | NONE => (exp, 1)::matched_list)
		[]
		subexps
	end

    fun addCost groups =
	map 
	    (fn(exp, count) => (exp, count, Cost.exp2cost exp))
	    groups
    (* sort the groups with cost *)
    fun sortGroups groups = GeneralUtil.sort_compare greaterthan groups	
    and greaterthan ((_, count1, cost1),(_, count2, cost2)) = count1*cost1 > count2*cost2

    (* create intermediate and rewrite *)
    fun createIntermediateAndRewrite exp = 
	let
	    val id = Unique.unique "#redundant"
	    val id_exp = ExpBuild.var id
	in
	    (ExpBuild.equals (id_exp, exp),
	     ExpProcess.equation2rewrite (ExpBuild.equals (exp, id_exp)))
	end

    (* grab expression list *)
    fun grabExpressionList c = 
	let
	    val eqs = map ExpProcess.rhs (List.filter ExpProcess.isEquation (!(#exps c)))
	    val outputs = Util.flatmap (fn(out)=> (DOF.Output.condition out)::(DOF.Output.contents out)) (!(#outputs c))
	in
	    eqs @ outputs
	end
							   
in
fun removeRedundancy (c:DOF.class) =
    let
	(* compute the initial cost *)
	val cost_before = Cost.class2cost c

	(* create a listing of all the expressions from which we'll extract subexpressions *)
	val exps = grabExpressionList c
	val pattern = Match.anybuiltin ""

	(* full list of subexpressions is created with the findRecursive call that matches any function *)
	val full_subexps = Util.flatmap (fn(exp)=> Match.findRecursive (pattern, exp)) exps

	(*val _ = Util.log ("Found "^(i2s (List.length full_subexps))^" sub expressions in class " ^ (Symbol.name (#name c)))*)

	(* group each subexpression by count *)
	val grouped_subexps = groupSubExpressions full_subexps
	(* remove all with counts = 1 *)
	val redundant_subexps = List.filter (fn(_,count)=>count > 1) grouped_subexps
	(* add cost to the tuple *)
	val subexps_with_count_and_cost = addCost redundant_subexps
	(* sort the sub expressions so the most costly is on the top *)
	val sorted_subexps = sortGroups subexps_with_count_and_cost

	(* run a recursive call, if it can't find any sub expressions, then it will exit, otherwise it will perform the 
	 * first substitution and recursively try again. *)
	val _ = case sorted_subexps of
		    (exp, count, cost)::rest => 
		    let
			val (intermediate_eq, rewrite) = createIntermediateAndRewrite exp
			val _ = applyRewritesToClassInternals [rewrite] c
			val exps' = intermediate_eq::(!(#exps c))
			val _ = (#exps c) := exps'
			val _ = if DynamoOptions.isFlagSet "logredundancy" then
				    Util.log ("Expression '"^(e2ps exp)^"' with cost "^(i2s cost)^" appears "^(i2s count)^" times -> Cost before/after: "^(i2s cost_before)^"/"^(i2s (Cost.class2cost c)) ^ " in class " ^ (Symbol.name (#name c)))
				else
				    ()
			(* recursively call *)
			val _ = removeRedundancy c
		    in
			()
		    end
		  | nil => () (* all finished *)
    in
	()
    end
end

end
