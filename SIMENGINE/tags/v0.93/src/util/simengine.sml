(* Get the value of the environment variable SIMENGINE if set, otherwise set and return the variable based on the realpath of simEngine executable *)
fun getSIMENGINE () = 
    let
	val simenginevar = "SIMENGINE"

        (* OS.FileSys.fullpath may throw an exception but that means it couldn't find the location of this running executable,
           no idea what to do in that case, just let the exception stop the execution *)
	fun setSIMENGINE () =
          let
	    val {dir=exec_dir, file=exec_name} = OS.Path.splitDirFile(OS.FileSys.fullPath(CommandLine.name()))
            val simenginepath = OS.Path.getParent(exec_dir)
          in
            simenginepath before MLton.ProcEnv.setenv{name=simenginevar, value=simenginepath}
          end

    in
	case OS.Process.getEnv(simenginevar) of
	    SOME reg => reg
	  | NONE => setSIMENGINE()
    end
