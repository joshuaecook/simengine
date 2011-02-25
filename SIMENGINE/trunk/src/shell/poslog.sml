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

structure PosLog =
struct

datatype pos = FILE of {filename:string, filepath:string, line: int, column: int}
	     | CONSOLE of {line: int, column: int}
	     | NOPOS

fun new() = nil

fun addSystemPos (env as (l, g, poslog), pos) =
    (l, g, (pos)::poslog)

fun pos2str (FILE {filename, filepath, line, column}) =
    "'" ^ filename ^ "' @ " ^ (Int.toString line) ^ "." ^ (Int.toString column)
  | pos2str (CONSOLE {line, column}) =
    "STDIN @ " ^ (Int.toString line) ^ "." ^ (Int.toString column)
  | pos2str NOPOS =
    "no position information available"


local
    open JSON 
    open JSONExtensions
    val int = int o IntInf.fromInt
    val intVal = IntInf.toInt o intVal
in
fun toJSON (FILE {filename, filepath, line, column}) =
    JSONTypedObject ("PosLog.FILE",
		     object [("filename", string filename),
			     ("filepath", string filepath),
			     ("line", int line),
			     ("column", int column)])
  | toJSON (CONSOLE {line, column}) =
    JSONTypedObject ("PosLog.CONSOLE",
		     object [("line", int line),
			     ("column", int column)])
  | toJSON NOPOS = null

fun fromJSON json =
    case fromJSONTypedObject json
     of NONE => NOPOS
      | SOME (JSON_CONSTRUCTOR ("PosLog.FILE", x)) => fileFromJSON x
      | SOME (JSON_CONSTRUCTOR ("PosLog.CONSOLE", x)) => consoleFromJSON x
      | _ => NOPOS

and fileFromJSON json =
    FILE {filename = memberVal (json, "filename", stringVal),
	  filepath = memberVal (json, "filepath", stringVal),
	  line = memberVal (json, "line", intVal),
	  column = memberVal (json, "column", intVal)}
    handle Option => NOPOS

and consoleFromJSON json = 
    CONSOLE {line = memberVal (json, "line", intVal), 
	     column = memberVal (json, "column", intVal)}
    handle Option => NOPOS


end
	    

end
