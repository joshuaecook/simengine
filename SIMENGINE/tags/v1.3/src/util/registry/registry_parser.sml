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


signature DYNREG_PARSE = 
sig
  type registry = Registry.entry list

  val parse : string -> registry
end

structure DynRegParse:DYNREG_PARSE = 
struct 

  structure LrVals = LrValsFun(structure Token=LrParser.Token)
  structure Lex = LexFun(structure Tokens=LrVals.Tokens)

  structure P = JoinWithArg(structure ParserData = LrVals.ParserData
                            structure Lex=Lex
			    structure LrParser=LrParser)

  type registry = Registry.entry list

  type filearg = 
       { file_name:     string, 
	 file_path:     string,
	 line_head_pos: int ref,
	 column_number: int ref,
	 line_number: int ref}


  fun parse (filepath: string) =
      let 
	  val la    = Lex.UserDeclarations.newLexerState(filepath)
	  val file  = TextIO.openIn filepath
		 
	  val filename = OS.Path.mkCanonical  filepath
	  val filepath = OS.Path.dir filepath

	  open Printer

	  val _ = Logger.log_notice ($("Importing registry file '" ^ filepath ^ "'"))
	  fun get _ = TextIO.input file
	  fun parseerror(s, (line,col), p2) = 
	      let
		  val filearg = {file_name=(#file_name la), 
				 file_path=(#file_path la),
				 line_head_pos=ref 0,
				 column_number=ref 0,
				 line_number = ref 0}
				
		  val line = (Int.toString line)
		  val col = (Int.toString col)

		  val file_name = (#file_name la)
	      in
		  (Logger.log_data_error file_name (Printer.$(s ^ " on line "^line^", col "^col));
		   DynException.setErrored())
	      end

	  val lexer = LrParser.Stream.streamify (Lex.makeLexer get la)
	  val (registry, _) = P.parse(30,lexer,parseerror,())
	  val _ = TextIO.closeIn file
      in 
	   registry
      end 
	  handle LrParser.ParseError => DynException.stdException ("Parse error on registry file", "RegistryParser.parse", Logger.USER)
	       | _ => DynException.stdException ("Error occurred during parse of registry file", "RegistryParser.parse", Logger.USER)
	       
end
