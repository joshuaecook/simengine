(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

signature MODELVALIDATE =
sig

    (* Run checks on the model that aren't caught anywhere else *)
    val validate : DOF.model -> unit
    val validateLicensing : DOF.model -> unit

end
structure ModelValidate : MODELVALIDATE =
struct

datatype message_type = NOTICE | WARNING | ERROR

fun notice s = Logger.log_notice (Printer.$(s))
fun warning s = Logger.log_warning (Printer.$(s))
fun error s = (Logger.log_error (Printer.$(s)); DynException.setErrored())

(* using a function that extracts appropriate expressions from a class definition, apply a rewrite that will produce a logging message *)
fun executeTestOverModel class2exps rewriteInfo =
    let
	(* createRewrite will build a rewrite that will search for a particular matching expression *)
	fun createRewrite (find, messagefun, messagetype) =
	    let
		val test = NONE
		val action = case messagetype of
				 NOTICE => (fn(exp) => (notice(messagefun(exp)); exp))
			       | WARNING => (fn(exp) => (warning(messagefun(exp)); exp))
			       | ERROR => (fn(exp) => (error(messagefun(exp)); exp))
		val replace = Rewrite.ACTION (Symbol.symbol "createTest", action)
	    in
		{find=find, test=test, replace=replace}
	    end

	val rewrite = createRewrite rewriteInfo

	(* grab all the classes *)
	val classes = CurrentModel.classes()

	(* pull together all the expressions to run over *)
	val exps = Util.flatmap class2exps classes

	(* evaluate the rewrite *)
	val _ = map (Match.applyRewriteExp rewrite) exps
    in
	()
    end

(* discrete iterator test - find times when a state with a discrete iterator has something other than x[n+1] on left side *)
fun properLHSDiscreteState () =
    let
	val discreteIterators = List.filter (fn(_,iter_type)=> case iter_type of
								   DOF.DISCRETE _ => true
								 | _ => false)
					    (CurrentModel.iterators())

	fun classToExpsWithDiscreteIterators c = 
	    let
		val state_syms = Util.flatmap (fn(iter_sym,_)=>ClassProcess.class2statesbyiterator iter_sym c) discreteIterators

		val exps = 
		    Util.flatmap 
			(fn(sym)=> ClassProcess.symbol2exps c sym)
			state_syms
	    in
		map ExpProcess.lhs exps
	    end


	val find = Match.anysym_with_predlist 
		       [("testIterator",  fn(exp)=> case ExpProcess.exp2temporaliterator exp of
							SOME (_, Iterator.RELATIVE 1)=> false
						      | SOME (_, Iterator.RELATIVE 0)=> false (* setting this to true will cause pp iterators on discrete states not to work *)
						      | SOME (_, Iterator.RELATIVE _)=> true
						      | _ => false)]
		       (Symbol.symbol "#a")

	fun messageFun (exp as Exp.TERM (Exp.SYMBOL (sym, _))) = 
	    let
		val iter_sym = case ExpProcess.exp2temporaliterator exp of
				   SOME (iter_sym, _)=> iter_sym
				 | NONE => DynException.stdException("Invalid found expression without iterator", "ModelValidate.properLHSDiscreteState", Logger.INTERNAL)
	    in
		"Invalid temporal index on discrete state "^(Symbol.name sym)^". Discrete states must be defined as "^(Symbol.name sym)^"["^(Symbol.name iter_sym)^"+1] on the left hand side of equation"
	    end
	  | messageFun _ = DynException.stdException("Invalid found expression", "ModelValidate.properLHSDiscreteState", Logger.INTERNAL)
			   
	val messageType = ERROR
    in
	executeTestOverModel classToExpsWithDiscreteIterators (find, messageFun, messageType)
    end

fun noRHSDerivatives () =
    let
	fun class2rhsexps (c as {exps, outputs, ...}) = 
	    let
		fun output2exps output = (DOF.Output.condition output) :: (DOF.Output.contents output)
	    in
		map ExpProcess.rhs (!exps) @ 
		Util.flatmap output2exps (!outputs)
	    end

	val find = Match.anysym_with_predlist
		       [("testderivative", 
			 (fn(exp)=> case exp of
					Exp.TERM (Exp.SYMBOL (_, props)) => 
					(case Property.getDerivative props of
					     SOME (0, _) => false
					   | NONE => false
					   | _ => true)
				      | _ => false))]
		       (Symbol.symbol "#a")

	fun messageFun (exp as Exp.TERM (Exp.SYMBOL (sym, _))) =
	    "Invalid derivative reference on symbol '"^(Symbol.name sym)^"'.  Derivatives are currently not supported on the right-hand-side of equations.  Please visit the FAQ at www.simatratechnologies.com for more information."
	  | messageFun _ = 
	    DynException.stdException("Invalid found expression", "ModelValidate.noRHSDerivatives", Logger.INTERNAL)

	val messageType = ERROR
    in
	executeTestOverModel class2rhsexps (find, messageFun, messageType)
    end

fun notEmpty statesize =
    let
	val result = 
	    statesize > 0 orelse
	    let
		val top_class = CurrentModel.top_class()
		val outputs = !(#outputs top_class)
	    in
		List.length outputs > 0
	    end
    in
	if result then
	    ()
	else
	    error("Model has no states or outputs")
    end

fun undefinedSymbols (model_outline) = 
    let
	fun checkUndefinedSymbolsInClass (classname) =
	    let
		val (_,outline) = DOFOutline.class_to_outline model_outline classname
		val symbols = DOFOutline.class_to_undefined_symbols outline
	    in
		if SymbolSet.isEmpty symbols then
		    ()
		else
		    app (fn(sym)=>error("Quantity " ^ (Symbol.name sym) ^ " is undefined")) (SymbolSet.listItems symbols)
	    end
	    handle e => DynException.checkpoint "ModelValidate.undefinedSymbols.checkUndefinedSymbolsInClass" e
    in
	SymbolTable.appi (fn(key, _)=>checkUndefinedSymbolsInClass key) model_outline
    end
    handle e => DynException.checkpoint "ModelValidate.undefinedSymbols" e

fun noCycles (model_outline) =
    let
	val model_outline = DOFOutline.model_to_outline (CurrentModel.getCurrentModel())
    in
	case DOFOutline.model_to_cycles model_outline of
	    [] => () (* no cycles *)
	  | cycles => app (fn{classname, deps}=> error ("Cycle found in Model " ^ (Symbol.name classname) ^ " with the following quantites " ^ (Util.symlist2s deps))) cycles
    end
    handle e => DynException.checkpoint "ModelValidate.noCycles" e

fun verifyTarget Target.CPU = ()
  | verifyTarget Target.OPENMP = Features.verifyEnabled Features.MULTI_CORE
  | verifyTarget Target.CUDA = Features.verifyEnabled Features.GPU

fun verifySolver (DOF.CONTINUOUS (Solver.EXPONENTIAL_EULER _)) = Features.verifyEnabled Features.EXPONENTIAL_EULER
  | verifySolver (DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _)) = Features.verifyEnabled Features.BACKWARD_EULER
  | verifySolver _ = ()

fun validate (model as (classes, instance, sysprops))= 
    CurrentModel.withModel model
    (fn () => 
	let
	    (* perform some basic tests for licensing *)
	    (* check if the target is supported *)
	    val {target,...} = sysprops
	    val _ = verifyTarget target

	    (* check that the number of states is acceptable *)
	    val statesize = ModelProcess.model2statesize model
	    val _ = Features.verifyEnabled (Features.NUM_STATES statesize)

	    (* verify that the solvers are all supported *)
	    val _ = app (verifySolver o #2) (ModelProcess.returnContinuousIterators())
			    
	    (* for profiling, report expression counts *)
	    val _ = if DynamoOptions.isFlagSet "profile" then
			Cost.logModelCosts model
		    else
			()

	    (* verify that the model is not empty *)
	    val _ = notEmpty (statesize)
	    val _ = DynException.checkToProceed() (* see if any errors were thrown *)

	    (* verify that no symbols are left undefined *)
	    val model_outline = DOFOutline.model_to_outline (CurrentModel.getCurrentModel())
	    val _ = Profile.time "Finding undefined symbols ..." undefinedSymbols (model_outline)
	    val _ = DynException.checkToProceed() (* see if any errors were thrown *)

	    (* verify that the LHS of equations have difference equation terms that only reference [n+1] or [n] *)
	    val _ = properLHSDiscreteState ()
	    val _ = DynException.checkToProceed() (* see if any errors were thrown *)

	    (* verify that no derivatives are used on the rhs of equations or in the outputs *)
	    val _ = noRHSDerivatives ()
	    val _ = DynException.checkToProceed() (* see if any errors were thrown *)
		    
	    (* verify that there are no cycles present in the model *)
	    val _ = Profile.time "Finding cycles through submodels ..." noCycles (model_outline)
	    val _ = DynException.checkToProceed() (* see if any errors were thrown *)

	    val _ = CurrentModel.setCurrentModel model
	in
	    ()
	end)
    handle e => DynException.checkpoint "ModelValidate.validate" e

fun validateLicensing (model as (_, _, sysprops))= 
    CurrentModel.withModel model
    (fn () => 
	let
	    (* perform some basic tests for licensing *)
	    (* check if the target is supported *)
	    val {target,...} = sysprops
	    val _ = verifyTarget target

	    (* check that the number of states is acceptable *)
	    val _ = Features.verifyEnabled (Features.NUM_STATES (ModelProcess.model2statesize model))
			    
	    (* check that the number of iterators is acceptable *)
	    val _ = Features.verifyEnabled (Features.NUM_ITERATORS (List.length (ModelProcess.returnUserIterators())))

	    (* verify that the solvers are all supported *)
	    val _ = app (verifySolver o #2) (ModelProcess.returnContinuousIterators())
	in
	    ()
	end
    )

end
