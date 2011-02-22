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
