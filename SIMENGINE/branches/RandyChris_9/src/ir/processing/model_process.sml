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
    val model2statesizebyiterator : Symbol.symbol -> DOF.model -> int

end = struct

fun model2statesize (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesize (CurrentModel.classname2class classname)
    end

fun model2statesizebyiterator (iter:Symbol.symbol) (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesizebyiterator iter (CurrentModel.classname2class classname)
    end

fun pruneIterators (model:DOF.model as (classes, top_inst, properties)) =
    let
	val {iterators, time, precision} = properties
	val iterators' = List.filter 
			     (fn(itersym,_) => 
				     model2statesizebyiterator itersym model > 0) iterators
	val properties' = {iterators=iterators',
			   time=time,
			   precision=precision}
	val model' = (classes, top_inst, properties')
    in
	CurrentModel.setCurrentModel(model')
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

	val (classes, _, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

	(* assign correct scopes for each symbol *)
	val _ = Util.log ("Assigning correct scope ...")
	val () = app ClassProcess.assignCorrectScope (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())

	val _ = Util.log ("Propagating iterators ...")
	val () = app ClassProcess.propagateIterators (CurrentModel.classes())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())

	val _ = Util.log ("Pruning excess iterators ...")
	val () = pruneIterators (CurrentModel.getCurrentModel())
	val () = DOFPrinter.printModel (CurrentModel.getCurrentModel())

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
	val _ = Util.log ("Ordering model ...")
	val _ = Ordering.orderModel(CurrentModel.getCurrentModel())

	val _ = DynException.checkToProceed()

	(* remap all names into names that can be written into a back-end *)
	val _ = Util.log ("Fixing symbol names ...")
	val () = (app ClassProcess.fixSymbolNames (CurrentModel.classes()))

	val _ = DynException.checkToProceed()
    in
	() (* all changes are in the model, we don't have to return this model *)
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeModel" e

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

	val _ = Util.log ("Adding EP index to class ...")
	val () = app (ClassProcess.addEPIndexToClass false) (CurrentModel.classes())
	val top_class = CurrentModel.classname2class (#classname (CurrentModel.top_inst()))
	val () = ClassProcess.addEPIndexToClass true top_class

	(*val () = app ClassProcess.fixStateSymbolNames (CurrentModel.classes())*)

	val _ = DynException.checkToProceed()
    in
	()
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeParallelModel" e

end
