structure ModelProcess : sig

    (* Primary functions to execute optimizations and commands across all classes within a model *)

    (* optimizeModel: algebraic and performance optimizations all occur in this function.  All transformations
      performed here should be independent of back-end.  If the data structure is to be saved prior to writing 
      a particular back-end, the data structure returned from optimizeModel would be a good one to save.  *)
    val optimizeModel : DOF.model -> unit

    (* normalizeModel and normalizeParallelModel: a normalization step for writing into a C back-end.  This 
      function performs transformations that are used solely for fitting within a back-end.  This
      can include renaming symbols to fit within compiler rules or adding code generation flags. *)
    val normalizeModel : DOF.model -> unit
    val normalizeParallelModel : DOF.model -> unit

    (* model2statesizebyiterator: Computes the total state space of the model on a per iterator basis *)
    val model2statesize : DOF.model -> int
    val model2statesizebyiterator : DOF.systemiterator -> DOF.model -> int

    (* createIteratorForkedModels: Creates a structure list of models that are unique by iterator *)
    val createIteratorForkedModels : DOF.model -> {top_class: Symbol.symbol,
						   iter: DOF.systemiterator,
						   model: DOF.model} list

    val duplicateModel : DOF.model -> (Symbol.symbol -> Symbol.symbol) -> DOF.model
    val pruneModel : (DOF.systemiterator option) -> DOF.model -> unit
	
    (* Iterator related functions - these all grab the iterators from CurrentModel *)
    val returnIndependentIterators : unit -> DOF.systemiterator list
    val returnDependentIterators : unit -> DOF.systemiterator list
    val returnStatefulIterators : unit -> DOF.systemiterator list
    val returnStatelessIterators : unit -> DOF.systemiterator list

    val hasUpdateIterator : Symbol.symbol -> bool
    val hasPostProcessIterator : Symbol.symbol -> bool

    (* Indicates whether an iterator is dependent upon another. *)
    val isDependentIterator : DOF.systemiterator -> bool
    val isImmediateIterator : DOF.systemiterator -> bool
    val isStatelessIterator : DOF.systemiterator -> bool

    val isDebugging : DOF.model -> bool
    val isProfiling : DOF.model -> bool

    val to_json : DOF.model -> mlJS.json_value

end = struct

fun log str = if DynamoOptions.isFlagSet "logdof" then 
		  Util.log str
	      else
		  ()

val i2s = Util.i2s

fun isDependentIterator (_, DOF.UPDATE _) = true
  | isDependentIterator (_, DOF.POSTPROCESS _) = true
  | isDependentIterator _ = false

fun isImmediateIterator (_, DOF.IMMEDIATE) = true
  | isImmediateIterator _ = false

fun isStatefulIterator (_, DOF.CONTINUOUS _) = true
  | isStatefulIterator (_, DOF.DISCRETE _) = true
  | isStatefulIterator (_, DOF.POSTPROCESS _) = true
  | isStatefulIterator _ = false

fun isStatelessIterator (_, DOF.UPDATE _) = true
  | isStatelessIterator (_, DOF.IMMEDIATE) = true
  | isStatelessIterator _ = false

val isStatefulIterator = not o isStatelessIterator

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
    
fun hasPostProcessIterator iter_sym =
    let
	val iterators = CurrentModel.iterators()
    in
	List.exists (fn(_,iter_type)=>case iter_type of
					  DOF.POSTPROCESS v => v=iter_sym
					| _ => false) iterators
    end
    

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
	val {iterators, precision, target, num_models, debug, profile} = properties

	fun filter_iter iterator =
	    0 < model2statesizebyiterator iterator model orelse
	    List.exists
		(fn (class) => 0 < List.length (ClassProcess.outputsByIterator iterator class))
		classes

	val iterators' = List.filter filter_iter iterators

	val properties' = {iterators=iterators',
			   precision=precision,
			   target=target,
			   num_models=num_models,
			   debug=debug,
			   profile=profile}
	val model' = (classes, top_inst, properties')
    in
	CurrentModel.setCurrentModel(model')
    end

fun applyRewritesToModel rewrites (model as (classes,_,_)) =
    app (fn(c)=>ClassProcess.applyRewritesToClass rewrites c) classes

fun fixTemporalIteratorNames (model as (classes, inst, props)) =
    let
	val {iterators,precision,target,num_models,debug,profile} = props
	val iterators' =  map 
			      (fn(iter_sym, iter_type)=>
				 (Util.sym2codegensym iter_sym,
				  case iter_type of
				      DOF.UPDATE v => DOF.UPDATE (Util.sym2codegensym v)
				    | DOF.POSTPROCESS v => DOF.POSTPROCESS (Util.sym2codegensym v)
				    | _ => iter_type))
			      iterators
			      
	val iter_name_map = map
				(fn((sym,_),(sym',_))=>(sym, sym'))
				(ListPair.zip (iterators, iterators'))

	val rewrites = map
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

	val _ = applyRewritesToModel rewrites model
	val {iterators,precision,target,num_models,debug,profile} = props
	val props'={iterators=iterators',precision=precision,target=target,num_models=num_models,debug=debug,profile=profile}
    in
	(classes, inst, props')
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

	(* expand out delays *)
	val _ = log ("Adding delays to difference equations")
	val () = app ClassProcess.addDelays (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()

	(* add intermediates for update equations if required - they are reading and writing to the same vector so we have to make sure that ordering doesn't matter. *)
	val _ = log ("Adding buffered intermediates ...")
	val () = app ClassProcess.addBufferedIntermediates (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()

	val _ = log ("Assigning correct scope ...")
	val () = app ClassProcess.assignCorrectScope (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()

(*	val _ = log ("Propagating temporal iterators ...")
	val () = app ClassProcess.propagatetemporalIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
*)
	val _ = log ("Propagating spatial iterators ...")
	val () = app ClassProcess.propagateSpatialIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()

	val _ = log ("Pruning excess iterators ...")
	val () = pruneIterators (CurrentModel.getCurrentModel())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())
	val _ = DynException.checkToProceed()

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
	val _ = log ("Ordering model ...")
	val _ = Ordering.orderModel(CurrentModel.getCurrentModel())

	val _ = DynException.checkToProceed()

	(* remap all names into names that can be written into a back-end *)
	val _ = log ("Fixing symbol names ...")
	(*val model' = fixTemporalIteratorNames(CurrentModel.getCurrentModel())
	val _ = CurrentModel.setCurrentModel(model')*)
	val () = (app ClassProcess.fixSymbolNames (CurrentModel.classes()))
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())


	val _ = DynException.checkToProceed()
    in
	() (* all changes are in the model, we don't have to return this model *)
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeModel" e

fun forkModelByIterator model (iter as (iter_sym,_)) = 
    let
	fun namechangefun iter_sym = (fn(name)=> Symbol.symbol ((Symbol.name name) ^ "_" ^ (Symbol.name iter_sym)))
	val model' as (classes',_,_) = duplicateModel model (namechangefun iter_sym)
	val _ = pruneModel (SOME iter) model'
	val _ = map (ClassProcess.updateForkedClassScope iter) classes'
    in
	model'
    end

fun createIteratorForkedModels model =
    let
	val iterators = CurrentModel.iterators()
	fun forkedModel (iter as (iter_sym,_)) = 
	    let 
		val model' as (_, {name,classname},_) = forkModelByIterator model iter
	    in
		{top_class=classname,
		 iter=iter,
		 model=model'}
	    end
    in
	map forkedModel iterators
    end

fun normalizeParallelModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, _, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

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
(*	val _ = Ordering.orderModel(model)*)

	val _ = DynException.checkToProceed()

	(* remap all names into names that can be written into a back-end *)
	(*val () = app ClassProcess.fixSymbolNames (CurrentModel.classes())*)
	(* must be put into a different normalizeModel function *)

	val _ = log ("Adding EP index to class ...")
	val () = app (ClassProcess.addEPIndexToClass false) (CurrentModel.classes())
	val top_class = CurrentModel.classname2class (#classname (CurrentModel.top_inst()))
	val () = ClassProcess.addEPIndexToClass true top_class

	(*val () = app ClassProcess.fixStateSymbolNames (CurrentModel.classes())*)

	(* Just some debug code ...*)
	val forkedModels = createIteratorForkedModels model
	val prevModel = CurrentModel.getCurrentModel()
	val iter_count = List.length (CurrentModel.iterators())
	val _ = app
		    (fn({top_class,iter=(iter_sym,_),model=model'},n)=>		       
		       (CurrentModel.setCurrentModel(model');
			log("\n==================   Iterator '"^(Symbol.name iter_sym)^"' ("^(i2s (n+1))^" of "^(i2s iter_count)^") =====================");
			DOFPrinter.printModel model'))
		    (StdFun.addCount forkedModels)
	val _ = CurrentModel.setCurrentModel(prevModel)

	val _ = DynException.checkToProceed()
    in
	()
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeParallelModel" e


fun isDebugging model = 
    let val (_, _, props : DOF.systemproperties) = model
    in #debug props
    end

fun isProfiling model =
    let val (_, _, props : DOF.systemproperties) = model
    in #profile props
    end

local open mlJS in
fun to_json (model as (classes,instance,properties)) =
    let val json_classes
	  = js_array (map ClassProcess.to_json classes)
	    
	val json_instance 
	  = let val {name,classname}
		  = instance
		val js_name 
		  = case name 
		     of SOME n => js_string (Symbol.name n) 
		      | NONE => js_null
	    in
		js_object [("name",js_name),
			   ("classname",js_string (Symbol.name classname))]
	    end

	val json_properties
	  = let val {iterators,precision,target,num_models,debug,profile}
		  = properties

		fun iterator_to_json (name, typ) = 
		    js_object (("name",js_string (Symbol.name name)) ::
			       (case typ
				 of DOF.CONTINUOUS solver => [("type", js_string "CONTINUOUS"),
							      ("solver", js_object (("name", js_string (Solver.solver2name solver)) ::
										    (map (fn (key,value) => (key, js_string value)) (Solver.solver2params solver))))]
				  | DOF.DISCRETE {sample_period} => [("type",js_string "DISCRETE"),
								     ("sample_period",js_float sample_period)]
				  | DOF.UPDATE parent => [("type",js_string "UPDATE"),
							  ("parent",js_string (Symbol.name parent))]
				  | DOF.POSTPROCESS parent => [("type",js_string "UPDATE"),
							       ("parent",js_string (Symbol.name parent))]
				  | DOF.IMMEDIATE => [("type", js_string "IMMEDIATE")]))
		    
		fun target_to_json Target.CPU = js_object [("type",js_string "CPU")]
		  | target_to_json Target.OPENMP = js_object [("type",js_string "OPENMP")]
		  | target_to_json (Target.CUDA {compute,multiprocessors,globalMemory})
		    = js_object [("type",js_string "CUDA"),
				 ("computeCapability",js_string (case compute of Target.COMPUTE11 => "1.1" | Target.COMPUTE13 => "1.3")),
				 ("globalMemory",js_int globalMemory)]

		val js_iterators
		  = js_array (map iterator_to_json iterators)
	    in
		js_object [("iterators",js_iterators),
			   ("precision",js_string (case precision of DOF.SINGLE => "float" | DOF.DOUBLE => "double")),
			   ("target",target_to_json target),
			   ("num_models",js_int num_models),
			   ("debug",js_boolean debug),
			   ("profile",js_boolean profile)]
	    end
    in
	js_object [("classes",json_classes),
		   ("instance",json_instance),
		   ("properties",json_properties)]
    end


end

end
