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

structure Globals =
struct

(* global status *)
val eof_encountered = ref false

val core_init = ref true

val lastline_yypos = ref 0
val line_count = ref 0

(* this type is required because new() does not return enough information to create a properly sized reference *)
val base_env: (KEC.exp Env.env ref * (KEC.exp Env.env * KEC.exp Env.env * KEC.exp Env.env option ref) * PosLog.pos list) ref 
  = ref (ref (Env.new()), (Env.new(), Env.new(), ref NONE), PosLog.new())



(* global constants *)
val simatra_url = "http://www.simatratechnologies.com"

fun CurrentDateTime () = (Date.fromTimeLocal (Posix.ProcEnv.time ())) : Date.date
val compile_date = CurrentDateTime()
val year = Date.year (CurrentDateTime())

val copyright = "Copyright "^(Int.toString year)^" Simatra Modeling Technologies, L.L.C."

val short_name = "simEngine"
val name = short_name ^ " Dynamical System Compiler"
(*val version = BuildOptions.version*)
val version = (GeneralUtil.int2str BuildOptions.majorVersion) ^ "." ^ (GeneralUtil.int2str BuildOptions.minorVersion) ^ BuildOptions.versionRevision
val extension = ".dso"
val edition = ref "N/A"
val licenseHolder = ref "N/A"
val expirationString = ref ""
val daysToString = ref (fn(d:Date.date)=>"")

fun buildDateAsDate () = 
    let
	val days = Int.toLarge (BuildOptions.buildDate)
	val seconds = days * 24 * 3600
    in
	(Date.fromTimeUniv o Time.fromReal o Real.fromLargeInt) seconds
    end    

fun buildDate () = (!daysToString) (buildDateAsDate())

(*
fun startupMessage () =
    (name ^ " v" ^ version ^ ", " ^ (!edition) ^ " Edition" ^ "\n"
     ^ (!licenseHolder) ^ " " ^ (!expirationString) ^ "\n"
     ^ "[built: " ^ (buildDate()) ^ "]" ^ (if BuildOptions.build = "unknown" then "" else (" " ^ BuildOptions.build)) ^ "\n"
     ^ copyright)
*)
(* remove licensing information from startup message *)
fun startupMessage () =
    (name ^ " v" ^ version ^ "\n"
     ^ "[built: " ^ (buildDate()) ^ "]" ^ (if BuildOptions.build = "unknown" then "" else (" " ^ BuildOptions.build)) ^ "\n"
     ^ copyright)
    

val path = ref let
 	       val env = Posix.ProcEnv.environ()
	       val pathstr = case List.find (String.isPrefix "PATH") env of
				 NONE => (DynException.stdException ("Host's environment does not contain a path", "Globals.path", Logger.DATA))
			       | SOME p => p
	       val paths = List.nth(String.fields(fn(c) => c = #"=") pathstr, 1)
	   in
	       String.fields(fn(c) => c = #":") paths  
	   end


end
