signature PARSE_JSON = sig
    val parse: TextIO.instream -> JSON.json
    val parseFile: string -> JSON.json
end

structure ParseJSON: PARSE_JSON = struct
structure L = LexJSON
structure T = JSONToken
structure J = JSON

fun parseValue lex = 
    case lex ()
     of T.NULL => J.null
      | T.TRUE => J.bool true
      | T.FALSE => J.bool false
      | T.INT z => J.int z
      | T.REAL r => J.real r
      | T.STRING s => J.string s
      | T.LARRAY => parseArray lex
      | T.LOBJECT => parseObject lex
      | token => raise Fail ("Token " ^ (T.toString token) ^ " does not represent a value")

and parseArray lex = J.array nil

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
	J.object (List.rev (loop nil))
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
	val lex = L.makeLexer input
    in
	parseValue lex
    end

fun parseFile filename =
    let val instream = TextIO.openIn filename
    in 
	parse instream before TextIO.closeIn instream
    end

end
