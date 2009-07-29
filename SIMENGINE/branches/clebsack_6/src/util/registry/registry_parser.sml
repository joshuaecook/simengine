
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
		  print ("ERROR @ " ^ file_name ^":" ^ line ^ ":" ^ col ^ ": " ^ s ^ "\n")
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
