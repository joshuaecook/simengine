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

  val back_buffer = ref NONE

  val buffer_stack = ref nil

fun push_buffer () =
    (buffer_stack := (!back_buffer) :: (!buffer_stack);
     back_buffer := NONE)

fun pop_buffer () =
    case !buffer_stack of
	nil => ()
      | buffer::stack => (back_buffer := buffer;
			  buffer_stack := stack)
      

(* This function is passed to the lexer to retrieve data from the
  input stream.  It is a little complicated because the language stops
  parsing at each newline.  Any text received beyond it is tossed
  out (it is NOT put back into a stream).  Because of this, we will
  buffer text we receieve past newline. 
   *)

val terminator = #"\n"

val terminator_str = "\n"

fun get instream = 
    let 
	fun eq a b = a = b

	fun contains string c =
	    List.exists (eq c) (explode string)

	  fun process_str str =
	      if contains str terminator then
		  let
		      val fields = String.fields (eq terminator) str
		      val (first, rest) = (hd fields, String.concatWith terminator_str (tl fields))
		  in
		      first ^ terminator_str before (if rest <> "" then
								     back_buffer := SOME rest
								 else
								     back_buffer := NONE)
		  end
	      else
		  (back_buffer := NONE;
		   str)

	  fun get_input () =
	      let 
		  val input = TextIO.input instream
	      in
		  process_str input
	      end
		  
		  

	  fun get_text _ = 
	      let
		  val str = 
		      case !back_buffer of
			  SOME str => 
			  process_str str
			| NONE =>
			  get_input()
	      in
		  str 
	      end
      in
	  get_text
      end

  fun parse (instream) =
      let 
	  val la    = Lex.UserDeclarations.newLexerState()

	  val _ = ParserSettings.lastLinePos := 0

	  (* TODO: better characterize syntax errors to distinguish from general user errors *)
	  fun parseerror(text, pos1, pos2) = 
	       Logger.log_usererror [pos2, pos1] ($ text)
	      
	  val lexer = LrParser.Stream.streamify (Lex.makeLexer (get instream) la)
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
