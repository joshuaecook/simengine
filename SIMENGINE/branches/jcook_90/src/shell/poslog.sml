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
    val int = int o IntInf.fromInt
    val intVal = IntInf.toInt o intVal
in
fun toJSON (FILE {filename, filepath, line, column}) =
    object [("$type", string "PosLog.FILE"),
	    ("filename", string filename),
	    ("filepath", string filepath),
	    ("line", int line),
	    ("column", int column)]
  | toJSON (CONSOLE {line, column}) =
    object [("$type", string "PosLog.CONSOLE"),
	    ("line", int line),
	    ("column", int column)]
  | toJSON NOPOS = null

fun fromJSON json =
    if isNull json then NOPOS
    else
	case memberValue (json, "$type", toString)
	 of SOME "PosLog.FILE" =>
	    FILE {filename = memberVal (json, "filename", stringVal),
		  filepath = memberVal (json, "filepath", stringVal),
		  line = memberVal (json, "line", intVal),
		  column = memberVal (json, "column", intVal)}
	  | SOME "PosLog.CONSOLE" =>
	    CONSOLE {line = memberVal (json, "line", intVal), 
		     column = memberVal (json, "column", intVal)}
	  | _ => raise Fail "Unrecognized type of JSON data"
end
	    

end
