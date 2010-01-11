functor ParseJSON (S: JSON_PARSE_STRUCTS): JSON_PARSE = struct
open S

structure T = Token

fun parseValue lex =
    case lex ()
     of T.NULL => JS.null
      | T.TRUE => JS.bool true
      | T.FALSE => JS.bool false
      | T.INT z => JS.int z
      | T.REAL r => JS.real r
      | T.STRING s => JS.string s
      | T.LARRAY => parseArray lex
      | T.LOBJECT => parseObject lex
      | token => raise Fail ("Token " ^ (T.toString token) ^ " does not represent a value")

and parseArray lex =
    let fun loop elements =
	    let val value = parseValue lex
	    in case lex ()
		of T.COMMA => loop (value :: elements)
		 | T.RARRAY => value :: elements
		 | token => raise Fail ("Invalid token " ^ (T.toString token) ^ " when trying to parse array")
	    end
    in
	JS.array (List.rev (loop nil))
    end

and parseObject lex =
    let fun loop members =
	    let val pair = parsePair lex
	    in
		case lex ()
		 of T.COMMA => loop (pair :: members)
		  | T.ROBJECT => pair :: members
		  | token => raise Fail ("Invalid token " ^ (T.toString token) ^ " when trying to parse object")
	    end
    in
	JS.object (List.rev (loop nil))
    end

and parsePair lex =
    case lex ()
     of T.STRING name =>
	(case lex ()
	  of T.COLON =>
	     let val value = parseValue lex
	     in
		 (name, value)
	     end
	   | token => raise Fail ("Invalid token " ^ (T.toString token) ^ " when trying to parse name/value pair"))
      | token => raise Fail ("Invalid token " ^ (T.toString token) ^ " when trying to parse name/value pair")

fun parse instream =
    let fun input n = TextIO.inputN (instream, n)
	val lex = Lex.makeLexer input
    in
	parseValue lex
    end

fun parseFile filename =
    let val instream = TextIO.openIn filename
    in
	parse instream before TextIO.closeIn instream
    end

end
