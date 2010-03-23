structure ModelProcess : sig

    (* Primary functions to execute optimizations and commands across all classes within a model *)

    (* Flattens all equations and instances into a single, monolithic model. *)
    val unify : DOF.model -> DOF.model

    (* optimizeModel: algebraic and performance optimizations all occur in this function.  All transformations
      performed here should be independent of back-end.  If the data structure is to be saved prior to writing 
      a particular back-end, the data structure returned from optimizeModel would be a good one to save.  *)
    val optimizeModel : DOF.model -> unit

    (* normalizeModel and normalizeParallelModel: a normalization step for writing into a C back-end.  This 
      function performs transformations that are used solely for fitting within a back-end.  This
      can include renaming symbols to fit within compiler rules or adding code generation flags. *)
    val normalizeModel : DOF.model -> unit

    (* prune unused iterators from the system *)
    val pruneIterators : DOF.model -> unit

    (* model2statesizebyiterator: Computes the total state space of the model on a per iterator basis *)
    val model2statesize : DOF.model -> int
    val model2statesizebyiterator : DOF.systemiterator -> DOF.model -> int

(*    (* createIteratorForkedModels: Creates a structure list of models that are unique by iterator *)
    val createIteratorForkedModels : DOF.model -> {top_class: Symbol.symbol,
						   iter: DOF.systemiterator,
						   model: DOF.model} list
*)
    val duplicateModel : DOF.model -> (Symbol.symbol -> Symbol.symbol) -> DOF.model
    val pruneModel : (DOF.systemiterator option) -> DOF.model -> unit

	
    (* Iterator related functions - these all grab the iterators from CurrentModel *)
    val returnContinuousIterators : unit -> DOF.systemiterator list
    val returnUserIterators : unit -> DOF.systemiterator list
    val returnIndependentIterators : unit -> DOF.systemiterator list
    val returnDependentIterators : unit -> DOF.systemiterator list
    val returnStatefulIterators : unit -> DOF.systemiterator list
    val returnStatelessIterators : unit -> DOF.systemiterator list

    val hasUpdateIterator : Symbol.symbol -> bool
    val hasAlgebraicIterator : Symbol.symbol -> bool
    val requiresMatrixSolution : DOF.systemiterator -> bool

    (* Returns the list of algebraic iterators matching a given name. *)
    val algebraicIterators: Symbol.symbol -> DOF.systemiterator list

    (* Indicates whether an iterator is dependent upon another. *)
    val isDependentIterator : DOF.systemiterator -> bool 
    val isImmediateIterator : DOF.systemiterator -> bool
    (* Indicates whether an iterator can produce an output - all iterators except UPDATE *)
    val isOutputIterator : DOF.systemiterator -> bool
    val isStatefulIterator : DOF.systemiterator -> bool
    val isStatelessIterator : DOF.systemiterator -> bool

    val isDebugging : DOF.model -> bool
    val isProfiling : DOF.model -> bool

end = struct


fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  Logger.log_notice (Printer.$ str)

val i2s = Util.i2s

val e2s = ExpPrinter.exp2str
val e2ps = ExpPrinter.exp2prettystr

fun isContinuousIterator (_, DOF.CONTINUOUS _) = true
  | isContinuousIterator _ = false

fun isUserIterator (_, DOF.CONTINUOUS _) = true
  | isUserIterator (_, DOF.DISCRETE _) = true
  | isUserIterator _ = false

fun isDependentIterator (_, DOF.UPDATE _) = true
  | isDependentIterator (_, DOF.ALGEBRAIC _) = true
  | isDependentIterator _ = false

fun isImmediateIterator (_, DOF.IMMEDIATE) = true
  | isImmediateIterator _ = false

fun isOutputIterator (_, DOF.UPDATE _) = false
  | isOutputIterator _ = true

fun isStatefulIterator (_, DOF.CONTINUOUS _) = true
  | isStatefulIterator (_, DOF.DISCRETE _) = true
  | isStatefulIterator (_, DOF.ALGEBRAIC _) = true
  | isStatefulIterator _ = false

fun isStatelessIterator (_, DOF.UPDATE _) = true
  | isStatelessIterator (_, DOF.IMMEDIATE) = true
  | isStatelessIterator _ = false

val isStatefulIterator = not o isStatelessIterator

fun returnContinuousIterators () = 
    List.filter isContinuousIterator (CurrentModel.iterators())

fun returnUserIterators () = 
    List.filter isUserIterator (CurrentModel.iterators())

fun returnIndependentIterators () =
    List.filter (not o isDependentIterator) (CurrentModel.iterators ())

fun returnDependentIterators () =
    List.filter isDependentIterator (CurrentModel.iterators ())

fun returnStatelessIterators () =
    List.filter isStatelessIterator (CurrentModel.iterators ())

fun returnStatefulIterators () = 
    List.filter isStatefulIterator (CurrentModel.iterators ())

fun hasUpdateIterator iter_sym =
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(_,iter_type)=>case iter_type of
					  DOF.UPDATE v => v=iter_sym
					| _ => false) iterators
    end

(* Beginning at the top instance's class,
 * flatten the model into a single class. *)
fun unify model =
    let val (classes, instance, properties) : DOF.model = model
	val flatclass = CurrentModel.withModel model (fn _ => ClassProcess.unify (CurrentModel.classname2class (#classname instance)))
    in
	([flatclass], instance, properties)
    end


    
fun hasAlgebraicIterator iter_sym =
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(_,iter_type)=>case iter_type of
					  DOF.ALGEBRAIC (_,v) => v=iter_sym
					| _ => false) iterators
    end

fun algebraicIterators iterName =
    List.filter (fn (_, DOF.ALGEBRAIC (_, name)) => name = iterName | _ => false) (CurrentModel.iterators ())
    

fun pruneModel iter_sym_opt model = 
    CurrentModel.withModel model (fn _ =>
    let val (classes, {classname,...}, _) = model
	fun isTop {name,...} = name = classname
    in app (fn(c)=> ClassProcess.pruneClass (iter_sym_opt, isTop c) c) classes
    end)

fun duplicateClasses classes namechangefun = 
    let
	(* create name mapping *)
	val orig_names = map ClassProcess.class2orig_name classes
	val name_mapping = map (fn({name,...},orig_name)=> (name, namechangefun name, orig_name, namechangefun orig_name)) (ListPair.zip(classes, orig_names))

	(* duplicate the classes *)
	val new_classes = 
	    map (fn((c as {name,...},(_,new_name,_,new_orig_name)))=> 
		   let
		       val c' = ClassProcess.duplicateClass c new_name
		       val c'' = ClassProcess.updateRealClassName c' new_orig_name
		   in
		       c''
		   end) (ListPair.zip (classes, name_mapping))

	(* update all the references in instances - this is O(n^2) - check for performance issues... *)
	val _ = app
		    (fn(name, new_name, orig_name, new_orig_name)=> app (fn(c as {name=name',...}) => ClassProcess.renameInsts ((name, new_name),(orig_name,new_orig_name)) c) new_classes)
		    name_mapping
    in
	new_classes
    end

fun duplicateModel model namechangefun = 
    let
	val (classes, top_inst as {name, classname}, props) = model
	val classes' = duplicateClasses classes namechangefun
	val classname' = namechangefun classname
	val top_inst' =  {name=name, classname=classname'}
    in
	(classes', top_inst', props)
    end
	

(* TODO: this should really be called at the end right before code gen so we don't create too many classes.  I still need to add the pruning step... *)

(* duplicate all classes by iterator and then remove the original classes *)
fun duplicateByIterator (model:DOF.model as (orig_classes, top_inst, props)) =
    let
	val iterators = CurrentModel.iterators()

	val new_classes_by_iterator = 
	    Util.flatmap
		(fn(iter_sym, _)=> duplicateClasses orig_classes (fn(name)=> Symbol.symbol ((Symbol.name name) ^ "_" ^ (Symbol.name iter_sym))))
		iterators
    in
	(new_classes_by_iterator, top_inst, props)
    end

fun model2statesize (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesize (CurrentModel.classname2class classname)
    end

fun model2statesizebyiterator (iter:DOF.systemiterator) (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesizebyiterator iter (CurrentModel.classname2class classname)
    end

fun pruneIterators (model:DOF.model as (classes, top_inst, properties)) =
    let
	val {iterators, precision, target, parallel_models, debug, profile} = properties

	(* is there an algebraic iterator that matches a particular iterator *)
	val algebraic_iterators = List.filter (fn(_, iter_type)=> case iter_type of DOF.ALGEBRAIC _ => true | _ => false) iterators
	fun find_matching_algebraic_iterators (iterator as (iter_sym,_)) = 
	    List.filter (fn(_, iter_type)=> case iter_type of DOF.ALGEBRAIC (_, iter_sym') => iter_sym=iter_sym' | _ => false) algebraic_iterators

	fun filter_iter iterator =
	    0 < model2statesizebyiterator iterator model orelse
	    List.exists
		(fn (class) => 0 < List.length (ClassProcess.outputsByIterator iterator class))
		classes orelse
	    let

		val matching_iterators = find_matching_algebraic_iterators iterator					 
		val result = foldl (fn(iterator',result)=> if result then
							       true
							   else 
							       filter_iter iterator') false matching_iterators
		(* do a quick error check - can't have a variable time step continuous iterator *)
		val _ = if result then
			    case iterator of
				(iter_sym, DOF.CONTINUOUS solver) => if Solver.isVariableStep solver then
									 (Logger.log_error (Printer.$("If iterator "^(Symbol.name iter_sym)^" has no states, it can not use a variable time step solver.  Iterator "^(Symbol.name iter_sym)^" was set to use the "^(Solver.solver2shortname solver)^" solver."));
									  DynException.setErrored())
								     else
									 ()
			      | _ => ()
			else
			    ()

	    in
		result
	    end
	    
	(* here are the iterators with states or outputs *)
	val iterators' = List.filter filter_iter iterators

	(* remove undefined iterators *)
	fun convertUndefined (iter_sym, DOF.CONTINUOUS Solver.UNDEFINED) = 
	    let
		(* set ODE45 as a default solver, should work in most cases *)
		val solver' = Solver.ODE45 {dt=0.1, abs_tolerance=0.000001, rel_tolerance=0.001}
		val _ = Logger.log_warning (Printer.$("No solver specified for continuous iterator '"^(Symbol.name iter_sym)^"', using default ode45 solver"))
	    in
		(iter_sym, DOF.CONTINUOUS solver')
	    end
	  | convertUndefined iter = iter
	val iterators'' = map convertUndefined iterators'				      

	val properties' = {iterators=iterators'',
			   precision=precision,
			   target=target,
			   parallel_models=parallel_models,
			   debug=debug,
			   profile=profile}
	val model' = (classes, top_inst, properties')
    in
	CurrentModel.setCurrentModel(model')
    end
    handle e => DynException.checkpoint "ModelProcess.pruneIterators" e

fun applyRewritesToModel rewrites (model as (classes,_,_)) =
    app (fn(c)=>ClassProcess.applyRewritesToClass rewrites c) classes

fun fixTemporalIteratorNames (model as (classes, inst, props)) =
    let
	val {iterators,precision,target,parallel_models,debug,profile} = props
	val iterators' =  map 
			      (fn(iter_sym, iter_type)=>
				 (Util.sym2codegensym iter_sym,
				  case iter_type of
				      DOF.UPDATE v => DOF.UPDATE (Util.sym2codegensym v)
				    | DOF.ALGEBRAIC (processtype, v) => DOF.ALGEBRAIC (processtype, Util.sym2codegensym v)
				    | _ => iter_type))
			      iterators
			      
	val iter_name_map = map
				(fn((sym,_),(sym',_))=>(sym, sym'))
				(ListPair.zip (iterators, iterators'))

	(* replace temporal iterators *)
	val iterator_rewrites = map
				    (fn(sym,sym')=>
				       let
					   val pred = ("Matching:"^(Symbol.name sym), (fn(exp)=> case ExpProcess.exp2temporaliterator exp of
												     SOME (iter_sym,_) => iter_sym=sym
												   | NONE => false))
					   val find = Match.anysym_with_predlist [PatternProcess.predicate_anysymbol, pred] (Symbol.symbol "a")
					   val test = NONE
					   val replace = Rewrite.ACTION (sym', (fn(exp)=>ExpProcess.updateTemporalIteratorToSymbol (sym',Util.sym2codegensym) exp))
				       in
					   {find=find,
					    test=test,
					    replace=replace}
				       end)
				    iter_name_map

	fun updateSymbolName new_symbol (Exp.TERM (Exp.SYMBOL (prev_symbol, props))) = Exp.TERM (Exp.SYMBOL (new_symbol, Property.setRealName props prev_symbol))
	  | updateSymbolName new_symbol _ = DynException.stdException("Unexpected non symbol matched", "ModelProcess.fixTemporalIteratorNames", Logger.INTERNAL)
				    
	(* replace iterators used explicitly *)
	val symbol_rewrites = map
			      (fn(sym,sym')=>
				 let
				     val find = Match.asym sym
				     val replace = Rewrite.ACTION (sym', updateSymbolName sym')
				 in
				     {find=find,
				      test=NONE,
				      replace=replace}
				 end)
			      iter_name_map

	val _ = applyRewritesToModel iterator_rewrites model
	val _ = applyRewritesToModel symbol_rewrites model
	val {iterators,precision,target,parallel_models,debug,profile} = props
	val props'={iterators=iterators',precision=precision,target=target,parallel_models=parallel_models,debug=debug,profile=profile}
    in
	(classes, inst, props')
    end

fun requiresMatrixSolution (_,iter_type) = 
    case iter_type of 
	DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _)=> true
      | _ => false
    
(* requiresFlattening - only requires it if it requires a matrix solver *)
fun requiresFlattening () =
    let
	val iterators = CurrentModel.iterators()

	fun solverRequiresFlattening (_,iter_type) =
	    case iter_type of
		DOF.CONTINUOUS (Solver.LINEAR_BACKWARD_EULER _) => true
	      | DOF.CONTINUOUS (Solver.EXPONENTIAL_EULER _) => true
	      | DOF.CONTINUOUS (Solver.FORWARD_EULER _) => DynamoOptions.isFlagSet "aggregate"
	      | _ => false
    in
	(DynamoOptions.isFlagSet "flatten") orelse
	(List.exists solverRequiresFlattening iterators)
    end

			
fun optimizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()
	val (classes, _, _) = model

	val _ = map ClassProcess.optimizeClass classes

	val _ = DynException.checkToProceed()
    in
	()
    end

fun normalizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, {name,classname}, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

	val _ = if DynamoOptions.isFlagSet "generateMathematica" then
		    CurrentModel.withModel (CurrentModel.getCurrentModel())
		    (fn()=>
		       (log ("Creating Mathematica model description (first propagating state iterators) ...");
			app ClassProcess.propagateStateIterators (CurrentModel.classes()); (* pre-run the assignCorrectScope *)
			log ("Creating Mathematica model description (converting model to code) ...");
			Printer.progs2file (MathematicaWriter.model2progs (CurrentModel.getCurrentModel()), Symbol.name classname ^ ".nb")))
		else
		    ()

	(* assign correct scopes for each symbol *)
	val _ = log ("Creating event iterators ...")
	val () = app ClassProcess.createEventIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* expand out delays *)
	val _ = log ("Adding delays to difference equations")
	val () = app ClassProcess.addDelays (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	val _ = log ("Assigning correct scope ...")
	val () = app ClassProcess.assignCorrectScope (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

(*	val _ = log ("Propagating temporal iterators ...")
	val () = app ClassProcess.propagatetemporalIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
*)
	val _ = log ("Propagating spatial iterators ...")
	val () = app ClassProcess.propagateSpatialIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	val _ = log ("Pruning excess iterators ...")
	val () = pruneIterators (CurrentModel.getCurrentModel())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* generate all offsets for instances *)
	(*val () = app ClassProcess.generateOffsets classes*)

(*
	(* reorder all the statements *)
	val () = app 
		     (fn(class)=> 
			let
			    val eqs' = EqUtil.order_eqs (!(#eqs class))
			in
			    (#eqs class) := eqs'
			end) 
		     classes
	*)

	val _ = if requiresFlattening() then
		    let
			val _ = log ("Flattening model ...")
			val model' = unify(CurrentModel.getCurrentModel())
			val _ = CurrentModel.setCurrentModel(model')
			val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())
		    in
			()
		    end
		else
		    ()
		     

	val _ = log ("Ordering model ...")
	val _ = Ordering.orderModel(CurrentModel.getCurrentModel())
	val _ = Profile.mark()

	val _ = DynException.checkToProceed()

	(* remap all names into names that can be written into a back-end *)
	val _ = log ("Fixing symbol names ...")
	val model' = fixTemporalIteratorNames(CurrentModel.getCurrentModel())
	val _ = CurrentModel.setCurrentModel(model')
	val () = (app ClassProcess.fixSymbolNames (CurrentModel.classes()))
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = Profile.mark()


	val _ = DynException.checkToProceed()

	val _ = log ("Adding EP index to class ...")
	val () = app (ClassProcess.addEPIndexToClass false) (CurrentModel.classes())
	val top_class = CurrentModel.classname2class (#classname (CurrentModel.top_inst()))
	val () = ClassProcess.addEPIndexToClass true top_class

	val _ = DynException.checkToProceed()
	val _ = Profile.mark()

	(* add intermediates for update equations if required - they are reading and writing to the same vector so we have to make sure that ordering doesn't matter. *)
	val _ = log ("Adding buffered intermediates ...")
	val () = app ClassProcess.addBufferedIntermediates (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()



    in
	() (* all changes are in the model, we don't have to return this model *)
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeModel" e


fun isDebugging model = 
    let val (_, _, props : DOF.systemproperties) = model
    in #debug props
    end

fun isProfiling model =
    let val (_, _, props : DOF.systemproperties) = model
    in #profile props
    end


end
