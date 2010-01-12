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
	    

local open mlJS in
fun to_json (FILE {filename, filepath, line, column}) =
    js_object [("type", js_string "FILE"),
	       ("name", js_string filename),
	       ("path", js_string filepath),
	       ("line", js_int line),
	       ("column", js_int column)]
  | to_json (CONSOLE {line, column}) = 
    js_object [("type", js_string "CONSOLE"),
	       ("line", js_int line),
	       ("column", js_int column)]
  | to_json NOPOS = js_null
end

end
