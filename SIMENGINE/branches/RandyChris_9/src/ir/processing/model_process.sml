structure ModelProcess =
struct

fun model2statesize (model:DOF.model) =
    let
	val (_, {name,classname}, _) = model
    in
	ClassProcess.class2statesize (CurrentModel.classname2class classname)
    end

fun optimizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()
	val (classes, _, _) = model

	val _ = map ClassProcess.optimizeClass classes

	val _ = DynException.checkToProceed()
    in
	model
    end

fun normalizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, _, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

	(* assign correct scopes for each symbol *)
	val () = app ClassProcess.assignCorrectScope (CurrentModel.classes())

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

end
