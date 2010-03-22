signature MODELVALIDATE =
sig

    (* Run checks on the model that aren't caught anywhere else *)
    val validate : DOF.model -> unit
    val validateLicensing : DOF.model -> unit

end
structure ModelValidate : MODELVALIDATE =
struct

datatype message_type = NOTICE | WARNING | ERROR


(* using a function that extracts appropriate expressions from a class definition, apply a rewrite that will produce a logging message *)
fun executeTestOverModel class2exps rewriteInfo =
    let
	(* createRewrite will build a rewrite that will search for a particular matching expression *)
	fun createRewrite (find, messagefun, messagetype) =
	    let
		val test = NONE
		val action = case messagetype of
				 NOTICE => (fn(exp) => (Logger.log_notice (Printer.$(messagefun(exp))); exp))
			       | WARNING => (fn(exp) => (Logger.log_warning (Printer.$(messagefun(exp))); exp))
			       | ERROR => (fn(exp) => (Logger.log_error (Printer.$(messagefun(exp))); DynException.setErrored(); exp))
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
	    val _ = Features.verifyEnabled (Features.NUM_STATES (ModelProcess.model2statesize model))

	    (* verify that the solvers are all supported *)
	    val _ = app (verifySolver o #2) (ModelProcess.returnContinuousIterators())
			    
	    (* for profiling, report expression counts *)
	    val _ = if DynamoOptions.isFlagSet "profile" then
			let
			    val total_cost = Cost.model2cost model
			    val unique_cost = Cost.model2uniquecost model
			in
			    Util.log("Expression Total Cost: "^ (Util.i2s total_cost) ^ "; Unique Cost: " ^ (Util.i2s unique_cost))
			end
		    else
			()


	    (* verify that the terms on the LHS are appropriate *)
	    (* TODO: could this be elaborated on?  what is 'appropriate'? *)
	    val _ = properLHSDiscreteState ()

	    val modelClone = ModelProcess.duplicateModel model (fn(s) => s)
	    val _ = Ordering.orderModel modelClone

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
