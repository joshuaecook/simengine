open Printer

signature PARSE = 
sig
  type stms = KEC.stm list

  val parse : TextIO.instream -> stms
end

structure OOLCParse = 
struct 

  structure LrVals = LrValsFun(structure Token=LrParser.Token)
  structure Lex = LexFun(structure Tokens=LrVals.Tokens)

  structure P = JoinWithArg(structure ParserData = LrVals.ParserData
                            structure Lex=Lex
			    structure LrParser=LrParser)

  type stms = KEC.stm list

  exception ParserError

(* A line-oriented reader for interactive input. *) 
  fun line_reader instream = 
   fn n => 
      case TextIO.inputLine instream handle Size => raise ParserError 
       of SOME line => line | NONE => "" 

  fun parse (instream) =
      let 
	  val la    = Lex.UserDeclarations.newLexerState()

	  val _ = ParserSettings.lastLinePos := 0

	  (* TODO: better characterize syntax errors to distinguish from general user errors *)
	  fun parseerror(text, pos1, pos2) = 
	       Logger.log_error_with_position [pos2, pos1] ($ text)
	      
    (* TODO data input could be more efficient if not restricted to line-at-a-time reading. 
     * The grammar stops parsing at each newline, discarding any remaining data.  
     * The reader function here must not return more than a single line. *) 
 	  val lexer = LrParser.Stream.streamify (Lex.makeLexer (line_reader instream) la) 

	  val (code, _) = P.parse(if !ParserSettings.isConsole then 0 else 30,lexer,parseerror,())

			  
	  val _ = if (DynException.isErrored()) then
		      (if !(ParserSettings.isConsole) then
			   (*Logger.log_error ($("Error found when parsing from the console\n"))*)()
		       else 
			   Logger.log_error ($("Error(s) found when parsing file '"^(!ParserSettings.filepath)^"/"^(!ParserSettings.filename)^"'"));
		       DynException.checkToProceed())
		  else
		      ()
      in 
	  code
      end 
	  handle LrParser.ParseError => 
		 raise ParserError
		   
end
