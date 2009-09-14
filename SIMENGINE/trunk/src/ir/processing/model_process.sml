structure ModelProcess =
struct

fun model2statesize (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesize (CurrentModel.classname2class classname)
    end

fun normalizeModel (model:DOF.model) =
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
	val _ = Ordering.orderModel(model)

	val _ = DynException.checkToProceed()

	(* remap all names into names that can be written into a back-end *)
	val () = app ClassProcess.fixSymbolNames (CurrentModel.classes())

	val _ = DynException.checkToProceed()
    in
	model
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
	val () = app (ClassProcess.addEPIndexToClass false) (CurrentModel.classes())
	val top_class = CurrentModel.classname2class (#classname (CurrentModel.top_inst()))
	val () = ClassProcess.addEPIndexToClass true top_class

	(*val () = app ClassProcess.fixStateSymbolNames (CurrentModel.classes())*)

	val _ = DynException.checkToProceed()
    in
	model
    end
    handle e => DynException.checkpoint "ModelProcess.normalizeParallelModel" e

end
