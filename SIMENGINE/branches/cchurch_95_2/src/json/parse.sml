functor ParseJSON (S: JSON_PARSE_STRUCTS): JSON_PARSE = struct
open S

structure T = Token

exception NoValue of T.token

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
      | token => raise NoValue token

and parseArray lex =
    let fun loop elements =
	    let val value = parseValue lex
	    in case lex ()
		of T.COMMA => loop (value :: elements)
		 | T.RARRAY => value :: elements
		 | token => raise Fail ("Invalid token " ^ (T.toString token) ^ " when trying to parse array")
	    end
	    handle NoValue (T.RARRAY) => elements
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
    in
	parseValue (Lex.makeLexer input)
    end

fun parseFile filename =
    let val instream = TextIO.openIn filename
    in
	parse instream before TextIO.closeIn instream
    end

fun parseString string =
    let 
	val size = String.size string
	val pos: int ref = ref 0
	fun input n =
	    let val i = ! pos
	    in if size < i
	       then ""
	       else if size < n + i
	       then String.extract (string, i, NONE)
	       else String.extract (string, i, SOME (n + i))
	    end before pos := n + (! pos)
    in
	parseValue (Lex.makeLexer input)
    end
end
