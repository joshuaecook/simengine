(*
Copyright (C) 2011 by Simatra Modeling Technologies

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*)

open Printer

signature PARSE = 
sig
  type stms = KEC.stm list

  val parse : TextIO.instream -> stms
  exception ParserError
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
	       (Logger.log_error_with_position [pos2, pos1] ($ text);
		DynException.setErrored())
	      
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
