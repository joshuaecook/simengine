structure JSONToken = struct
datatype token = EOF
	       | COMMA
	       | COLON
	       | NULL
   	       | TRUE | FALSE
	       | INT of IntInf.int
	       | REAL of LargeReal.real
	       | STRING of string
	       | LARRAY | RARRAY
	       | LOBJECT | ROBJECT

val toString =
 fn EOF => "<eof>"
  | COMMA => ","
  | COLON => ":"
  | NULL => "null"
  | TRUE => "true"
  | FALSE => "false"
  | INT z => if 0 > z then "-" ^ (IntInf.toString (~z))
	     else IntInf.toString z
  | REAL r => if 0.0 > r then "-" ^ (LargeReal.toString (~r))
	      else LargeReal.toString r
  | STRING s => String.concat ("\"" :: s :: "\"" :: nil)
  | LARRAY => "["
  | RARRAY => "]"
  | LOBJECT => "{"
  | ROBJECT => "}"

end
