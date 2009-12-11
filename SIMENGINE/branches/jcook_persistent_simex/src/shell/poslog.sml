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
