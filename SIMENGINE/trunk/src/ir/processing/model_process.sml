structure ModelProcess =
struct


fun normalizeModel (model:DOF.model) =
    let
	val _ = DynException.checkToProceed()

	val (classes, _, _) = model
	(* TODO, write the checks of the model IR as they are needed *)

	(* generate all offsets for instances *)
	val () = app ClassProcess.generateOffsets classes

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

end
