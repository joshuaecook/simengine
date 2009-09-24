structure CompilerLib =
struct

val TypeMismatch = DynException.TypeMismatch
and IncorrectNumberOfArguments = DynException.IncorrectNumberOfArguments

exception Aborted

fun std_compile exec args =
    (case args of
	 [object] => 
	 (let
	      val _ = if DynException.isErrored() then
			  raise Aborted
		      else
			  ()

	      val forest = case ModelTranslate.translate(exec, object) of
				       SOME f => f
				     | NONE => raise Aborted
						  

	      val (classes, _, _) = forest

	      val _ = DOFPrinter.printModel forest   

	      val _ = CurrentModel.setCurrentModel forest

	      val _ = ModelProcess.normalizeModel (CurrentModel.getCurrentModel())

	      val _ = DOFPrinter.printModel (CurrentModel.getCurrentModel())

	      val _ = CurrentModel.setCurrentModel (ModelProcess.normalizeParallelModel (CurrentModel.getCurrentModel()))

	      val code = CParallelWriter.buildC (CurrentModel.getCurrentModel())

	      val _ = DynException.checkToProceed()
	  in 
	      case code of
		  CParallelWriter.SUCCESS => KEC.LITERAL(KEC.CONSTSTR "\nCompilation Finished Successfully\n")
		| CParallelWriter.FAILURE f => KEC.LITERAL(KEC.CONSTSTR ("\nFailure: " ^ f ^ "\n"))
	  end 
	  handle Aborted => KEC.LITERAL(KEC.CONSTSTR ("\nFailure: Compilation stopped due to errors\n")))
       | _ => raise IncorrectNumberOfArguments {expected=1, actual=(length args)})
    handle e => DynException.checkpoint "CompilerLib.std_compile" e

val library = [{name="compile", operation=std_compile}]

end
