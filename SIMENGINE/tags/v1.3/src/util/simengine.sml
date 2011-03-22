(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

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

fun getSIMENGINEDOL () =
    let
	val var = "SIMENGINEGLOBALDOL"
    in
	case OS.Process.getEnv var of
	    SOME reg => reg
	  | NONE => OS.Path.concat (getSIMENGINE(), OS.Path.fromUnixPath "data/global.dol")
    end

fun isFile file =
    (OS.FileSys.fileSize file;
     true)
    handle _ => false

fun getSIMENGINEPROFILEDOL () =
    let
	val file = "../datafiles/profile.dol"
    in
	if isFile file then
	    SOME file
	else
	    NONE
    end

fun getSIMENGINELOCALDOL () =
    let
	val var = "SIMENGINEDOL"
    in
	case OS.Process.getEnv var of
	    SOME reg => SOME reg
	  | NONE => (case OS.Process.getEnv "HOME" of
			 SOME home => 
			 let
			     val dol = OS.Path.concat (home, 
						       (OS.Path.fromUnixPath ".simatra/local.dol"))
			 in
			     if isFile dol then
				 SOME dol
			     else
				 NONE
			 end
		       | NONE => NONE)
    end

fun getUPDATEDOL () = 
    case OS.Process.getEnv "HOME" of
	SOME home => 
	let
	    val dol = OS.Path.concat (home, 
				      (OS.Path.fromUnixPath ".simatra/update.dol"))
	in
	    if isFile dol then
		SOME dol
	    else
		NONE
	end
      | NONE => NONE

fun getSIMENGINESEW () =
    let
	val var = "SIMENGINESEW"
    in
	case OS.Process.getEnv var of
	    SOME reg => reg
	  | NONE => OS.Path.concat (getSIMENGINE(), OS.Path.fromUnixPath "data/default.sew")
    end


fun getSIMENGINELOG () =
    let
	val var = "SIMENGINELOG"
    in
	case OS.Process.getEnv var of
	    SOME reg => reg
	  | NONE => 
	    let 
		val dir = case OS.Process.getEnv "TMPDIR" of SOME x => x | _ => OS.Path.fromUnixPath "/tmp"
		val file = case OS.Process.getEnv "USER" of SOME x => "simEngine-" ^ x ^ ".log" | _ => "simEngine.log"
	    in
		OS.Path.concat (dir, file)
	    end
    end
