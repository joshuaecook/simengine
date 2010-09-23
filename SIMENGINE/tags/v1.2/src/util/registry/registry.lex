open TextIO

structure Tokens = Tokens

(* This is the type passed to the lexer. *)
type lexarg = { file_name:     string, 
	        file_path:     string,
		line_head_pos: int ref,
		column_number: int ref,
		line_number: int ref}

type arg = lexarg

type ('a, 'b) token = ('a,'b) Tokens.token

(* Stuff needed by ML-Yacc *)
type pos = int * int
type svalue = Tokens.svalue
type lexresult  = (svalue,pos) Tokens.token

val maxIdLen = 32

fun newLexerState (input_file_path) = 
   ({file_name     = OS.Path.mkCanonical  input_file_path,
     file_path     = input_file_path,
     line_head_pos = ref 0,
     column_number = ref 0,
     line_number   = ref 0})
    
    
fun getpos {file_name, file_path, line_head_pos, column_number, line_number} yypos offset =
(!line_number, yypos - !line_head_pos + offset)

(* Mandatory error and end-of-file handlers. *)

fun error (err, {file_name, file_path, line_head_pos, column_number, line_number}: lexarg) =
    print(file_name ^ ": " ^ err ^ "\n") 

fun eof (_) =
    Tokens.EOF((0,0),(0,0))

(*fun ++ (a: int ref) = (a := !a + 1)
fun -- (a: int ref) = (a := !a - 1)
fun ^= (a: string ref, b: string) = (a := !a ^ b)

val debug = false
fun debugprint (s) = if debug then print (s ^ "\n") else ()
*)
%%

%s COMMENT;
%s STRING;

%arg (la as {file_name, file_path, line_head_pos, column_number, line_number});

%header (functor LexFun(structure Tokens: DYNREG_TOKENS));

DIGITS	= [0-9]+;
INT	= {DIGITS};
REAL 	= ({DIGITS}?("."{DIGITS})?)|({DIGITS}("."{DIGITS})?[eE][+-]?{DIGITS})|(Inf(inity)?);
ID	= [a-zA-Z][a-zA-Z0-9_]*;
STRING 	= \"[^\"]*\";
WS      = [\012\ \t];

%%
					  


<INITIAL>"true" => (Tokens.TRUE(getpos la yypos 0, getpos la yypos 4));
<INITIAL>"false" => (Tokens.FALSE(getpos la yypos 0, getpos la yypos 5));
<INITIAL>">"    => (Tokens.GTHAN(getpos la yypos 0, getpos la yypos 1));
<INITIAL>"<"    => (Tokens.LTHAN(getpos la yypos 0, getpos la yypos 1));
<INITIAL>"/"    => (Tokens.SLASH(getpos la yypos 0, getpos la yypos 1));
<INITIAL>"="    => (Tokens.EQ(getpos la yypos 0, getpos la yypos 1));
<INITIAL>"["    => (Tokens.LBRACKET(getpos la yypos 0, getpos la yypos 1));
<INITIAL>"]"    => (Tokens.RBRACKET(getpos la yypos 0, getpos la yypos 1));
<INITIAL>","    => (Tokens.COMMA(getpos la yypos 0, getpos la yypos 1));


<INITIAL>{INT} => ((* Rule to match integers. *)
		   let
                       val intval = (Int.fromString(yytext))
		   in
		       case intval of
			   SOME(a:int) => Tokens.INT(a, getpos la yypos 0, getpos la yypos (size yytext))
		   | NONE => (column_number := (yypos - !column_number);
			      print ("LEX ERROR: integer conversion error");
			      continue())
		   end);

<INITIAL>{REAL} => ((* Rule to match real numbers. *)
		   let
                       val realval = (Real.fromString(yytext))
		   in
		       case realval of
			   SOME(a:real) => Tokens.REAL(a, getpos la yypos 0, getpos la yypos (size yytext))
    		         | NONE => (column_number := (yypos - !column_number);
			       (*error (DLErrors.ERR_SYNTAX "real conversion error", la);*)
			       continue())
		   end);

<INITIAL>{STRING} => ((* Rule to match strings *)
		      let
			val s = substring (yytext, 1, (size yytext) - 2)
		      in
			Tokens.STRING (s, getpos la yypos 0, getpos la yypos (size s))
		      end);


		      <INITIAL>{ID}    => (Tokens.ID(yytext, getpos la yypos 0, getpos la yypos (size yytext)));


<INITIAL>(\n|\r\n) => ((* Rule to match new line. *)
		       line_number := !line_number + 1;
		       column_number := 0;
		       line_head_pos := yypos;
		       continue());

<INITIAL>{WS}+ => ((* Rule to match whitespace. *)
		   continue());

<INITIAL>"//".*(\n|\r\n)  =>  (continue());
<INITIAL>"/*"             =>  (YYBEGIN(COMMENT); continue());
<COMMENT>[^*\r\n]*               =>  (continue ());
<COMMENT>"*"+[^*/\r\n]*          =>  (continue ());
<COMMENT>(\n|\r\n)               =>  (continue());
<COMMENT>"*"+"/"                 =>  (YYBEGIN(INITIAL); continue ());

<COMMENT>.        => ((* Any unmatched characters in comments are ignored *)
		      continue ());

<INITIAL>.        => ((* Any unmatched characters produce an error. *)
		      (*error (DLErrors.ERR_SYNTAX ("unrecognized character `" ^ yytext ^ "'"), la);*)
		      continue());
		       
