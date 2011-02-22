signature SPIL_READER = sig
    structure Syntax: SPIL
    structure Context: PARSER_CONTEXT

    val read : (int * Context.context * TextIO.instream) -> Syntax.fragment
    val readFile : string -> Syntax.fragment
    val readStdin : unit -> Syntax.fragment
end

functor MakeSpilReader (structure Syntax: SPIL 
structure Context: PARSER_CONTEXT
structure Parser: ARG_PARSER
sharing type Parser.arg = Context.context
sharing type Parser.lexarg = Context.context
sharing type Parser.pos = Context.point
sharing type Parser.result = Syntax.fragment
structure Tokens: SpilGrammar_TOKENS): SPIL_READER = struct

structure Syntax = Syntax
structure Context = Context

fun parse (lookahead, context, input) =
    let
	val error =
	 fn (message, point, endpoint) =>
	    raise Fail (message^" in "^(Context.filename context)^" from "^(Context.printPoint point)^" to "^(Context.printPoint endpoint))

	fun invoke lexer =
	    Parser.parse (lookahead, lexer, error, context)

	fun loop lexer =
	    let 
		val (result, lexer) = invoke lexer
	    in
		result
	    end
    in
	loop (Parser.makeLexer input context)
    end

fun read (lookahead, context, stream) =
    parse (lookahead, context, fn n => TextIO.inputN (stream, n))

fun readStdin () =
    let
	val lookahead = 0
    in
	read (lookahead, Context.stdin, TextIO.stdIn)
    end

fun readFile filename =
    let 
	val lookahead = 10
	val stream = TextIO.openIn filename
    in
	read (lookahead, Context.file filename, stream) before TextIO.closeIn stream
    end

end
