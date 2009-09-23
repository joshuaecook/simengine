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

end
