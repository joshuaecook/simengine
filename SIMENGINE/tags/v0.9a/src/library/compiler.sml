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

(* Old writers are now deprecated - Carl
	      val _ = if DynamoOptions.isFlagSet "generateC" then
			  (Logger.log_notice (Printer.$("Generating Debug C Back-end"));
			  CWriter.buildC (CurrentModel.getCurrentModel()))
		      else
			  CWriter.SUCCESS
	      val _ = DynException.checkToProceed()

	      val _ = MexWriter.buildMex (CurrentModel.getCurrentModel())
	      val _ = DynException.checkToProceed()
	      val _ = ODEMexWriter.buildODEMex (CurrentModel.getCurrentModel())
	      val _ = DynException.checkToProceed()
*)
	      val _ = CurrentModel.setCurrentModel (ModelProcess.normalizeParallelModel (CurrentModel.getCurrentModel()))
	      (* code writers for parallel implementation follow ...*)
	      val code = (*if DynamoOptions.isFlagSet "generateC" then
			  (Logger.log_notice (Printer.$("Generating Debug C Back-end"));*)
			  CParallelWriter.buildC (CurrentModel.getCurrentModel())
(*		      else
			  CParallelWriter.SUCCESS*)
	      val _ = DynException.checkToProceed()

	      (*val code = System.SUCCESS*) (*ModelCompileLauncher.compile (name, forest)*)
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
