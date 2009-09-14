open TextIO
open Printer

structure Tokens = Tokens

(* This is the type passed to the lexer. *)
type lexarg = unit

type arg = lexarg

type ('a, 'b) token = ('a,'b) Tokens.token

(* Stuff needed by ML-Yacc *)
type pos = PosLog.pos
type svalue = Tokens.svalue
type lexresult  = (svalue,pos) Tokens.token

val maxIdLen = 32

val lastline_yypos = ParserSettings.lastLinePos

fun genpos (yypos) =
    if !ParserSettings.isConsole then 
	PosLog.CONSOLE {line = !ParserSettings.lineCount, column = yypos - !lastline_yypos}
    else
	PosLog.FILE {filename= !ParserSettings.filename, filepath= !ParserSettings.filepath,
 	             line = !ParserSettings.lineCount, column=yypos - !lastline_yypos}


(* List of keywords, followed by a list of the corresponding token type generators.  *) 
val keyword_table = [("function",         Tokens.FUNCTION),
		     ("multifunction",    Tokens.MULTIFUNCTION),
		     ("while",            Tokens.WHILE),
		     ("foreach",          Tokens.FOREACH),
		     ("forall",           Tokens.FORALL),
		     ("exists",           Tokens.EXISTS),
		     ("suchthat",         Tokens.SUCHTHAT),

		     ("property",         Tokens.PROPERTY),
 		     ("get",              Tokens.GET),
		     ("set",              Tokens.SET),
		     ("let",              Tokens.LET),

		     ("do",               Tokens.DO),
		     ("in",               Tokens.IN),
		     ("end",              Tokens.END),
		     ("error",            Tokens.ERROR),
		     ("var",              Tokens.VAR),
		     ("constant",         Tokens.CONSTANT),
		     ("global",           Tokens.GLOBAL),
		     ("operator",         Tokens.OPERATOR),
		     ("constructor",      Tokens.CONSTRUCTOR),
		     ("satisfies",        Tokens.SATISFIES),
		     ("interface",        Tokens.INTERFACE),
		     ("namespace",        Tokens.NAMESPACE),
		     ("extends",          Tokens.EXTENDS),
		     ("of",               Tokens.OF),
		     ("overload",         Tokens.OVERLOAD),
		     ("open",             Tokens.OPEN),
		     ("type",             Tokens.TYPE),
		     ("undefined",        Tokens.UNDEFINED),
		     
		     ("to",               Tokens.TO),
		     ("by",               Tokens.BY),
		     ("with",             Tokens.WITH),
		     ("stateful",         Tokens.STATEFUL),
		     ("tunable",          Tokens.TUNABLE),
		     ("visible",          Tokens.VISIBLE),
		     ("output",           Tokens.OUTPUT),
		     ("input",            Tokens.INPUT),
		     ("iterator",         Tokens.ITERATOR),
		     ("model",            Tokens.MODEL),
		     ("submodel",         Tokens.SUBMODEL),
		     ("parameter",        Tokens.PARAMETER),
		     ("quantity",         Tokens.QUANTITY),
		     ("state",            Tokens.STATE),
		     ("hidden",           Tokens.HIDDEN),
		     ("public",           Tokens.PUBLIC),

		     ("when",             Tokens.WHEN),
		     ("otherwise",        Tokens.OTHERWISE),
		     ("lambdafun",        Tokens.LAMBDAFUN),

		     ("enumeration",      Tokens.ENUMERATION),
		     ("equation",         Tokens.EQUATION),
		     ("equations",        Tokens.EQUATIONS),

		     ("LF",               Tokens.LF),
		     ("or",               Tokens.OR),
		     ("and",              Tokens.AND),

		     ("assert",           Tokens.ASSERT),
		     ("if",               Tokens.IF),
		     ("then",             Tokens.THEN),
		     ("else",             Tokens.ELSE),
		     ("elseif",           Tokens.ELSEIF),
		     ("class",            Tokens.CLASS),
		     ("import",           Tokens.IMPORT),
		     ("true",             Tokens.TRUE),
		     ("false",            Tokens.FALSE)]

(* finds a keyword in the table, and returns a generating function to make a Token for it *)
fun lookup keyword =
    case List.find (fn (key, _) => key = keyword) keyword_table
     of SOME (_, constructor) =>
	SOME (fn (pos) => constructor(genpos pos, genpos(pos + (String.size keyword))))
      | NONE => NONE


val removeUnderbars = String.translate (fn #"_" => "" | c => Char.toString c)

exception IllegalChar of char

(* string -> string -> string
   Verifies that of the characters in a string appear in a whitelist.
   Raises IllegalChar when a character is encountered that is not in the whitelist.
 *)
fun vet (whitelist: string) =
    let 
	val allow = Char.contains whitelist (* some implementations may optimize this partial application *)
    in
	String.translate (fn c => if allow c then String.str c else raise IllegalChar c)
    end

val vetBin = vet "01"
val vetOct = vet "01234567"
val vetHex = vet "0123456789abcdefABCDEF"
    
fun newLexerState _ = ()    

datatype lexState = COMMENT_STATE
		  | STRING_STATE
		  | PAREN_STATE
		  | BRACE_STATE
		  | CBRACE_STATE

fun lexStateToString COMMENT_STATE = "comment"
  | lexStateToString STRING_STATE = "string"
  | lexStateToString PAREN_STATE = "parentheses"
  | lexStateToString BRACE_STATE = "square brackets"
  | lexStateToString CBRACE_STATE = "curly braces"

val lexStateStack : lexState list ref = ref nil;

fun pushLexState state =
    lexStateStack := state :: (!lexStateStack)

fun popLexState () =
    lexStateStack := tl (!lexStateStack)

fun lexState () =
    case !lexStateStack 
     of s :: _ => SOME s
      | nil => NONE

(* Mandatory error and end-of-file handlers. *)

fun error pos text = 
    (Logger.log_usererror [pos] text; 
     DynException.setErrored())

fun eof () =
    (Globals.eof_encountered := true;
     Tokens.EOF(PosLog.NOPOS,PosLog.NOPOS))

%%

%s COMMENT STRING STRING_EXP;

%arg (unit);

%header (functor LexFun(structure Tokens: LC_TOKENS));

KEYWORD = (let | in | end | LF | val | assert | or | and | if | then | else | elseif | true | false | import | class | function | multifunction | while | foreach | do | var | constant | quantity | global | operator | constructor | satisfies | extends | namespace | open | type | overload | when | otherwise | of | lambdafun | undefined | enumeration | d | equation | equations | error | to | by | with | stateful | tunable | visible | output | input | iterator | model | submodel | submodels | parameter | parameters | state | states | public | hidden | forall | exists | suchthat | property | get | set);


DIGIT   = [0-9];
DIGITS	= {DIGIT}+;
LETTER  = [a-zA-Z];
INT	= {DIGITS};
REAL 	= ({DIGITS}?("."{DIGITS})?)|({DIGITS}("."{DIGITS})?[eE][+-]?{DIGITS})|(Inf(inity)?)|(NaN);
HEX     = 0x({DIGIT}|{LETTER})(({DIGIT}|{LETTER}|_)*({DIGIT}|{LETTER}))?;
OCT     = 0o({DIGIT}|{LETTER})(({DIGIT}|{LETTER}|_)*({DIGIT}|{LETTER}))?;
BIN     = 0b({DIGIT}|{LETTER})(({DIGIT}|{LETTER}|_)*({DIGIT}|{LETTER}))?;
ID	= [a-zA-Z]([a-zA-Z0-9_]*[a-zA-Z0-9])?;

EOL     = \n|\r\n|\r;
ESCAPE  = \\[^\r\n];
STRING_CHAR    = ([^\$\"\n\r\\]|{ESCAPE});

PATTERN = \|[^\|]*\|;
WS      = [\012\ \t];

%%
					  

<INITIAL>{KEYWORD} => ((* looks up a keyword in the keyword table and
			  returns it *)
		       let 
			 val tok = (case lookup yytext of
					NONE   => Tokens.ID(yytext, genpos yypos, genpos (yypos + (size yytext)))
				      | SOME t => t yypos)
		       in
			   tok
		       end);


<INITIAL>"_["   => (Tokens.UNDERBRACE(genpos yypos, genpos (yypos + 2)));
<INITIAL>"'"   => (Tokens.TICK(genpos yypos, genpos (yypos + 1)));

<INITIAL>"."   => (Tokens.PERIOD(genpos yypos, genpos (yypos + 1)));

<INITIAL>","   => (Tokens.COMMA(genpos yypos, genpos (yypos + 1)));

<INITIAL>"*"   => (Tokens.STAR(genpos yypos, genpos (yypos + 1)));
<INITIAL>"+"   => (Tokens.PLUS(genpos yypos, genpos (yypos + 1)));
<INITIAL>"-"   => (Tokens.MINUS(genpos yypos, genpos (yypos + 1)));
<INITIAL>"/"   => (Tokens.SLASH(genpos yypos, genpos (yypos + 1)));

<INITIAL>"%"   => (Tokens.PERCENT(genpos yypos, genpos (yypos + 1)));

<INITIAL>"&"   => (Tokens.AMPERSAND(genpos yypos, genpos (yypos + 1)));

<INITIAL>"=="   => (Tokens.EQ(genpos yypos, genpos (yypos + 2)));
<INITIAL>"<>"   => (Tokens.NEQ(genpos yypos, genpos (yypos + 2)));

<INITIAL>">"    => (Tokens.GT(genpos yypos, genpos (yypos + 1)));
<INITIAL>"<"    => (Tokens.LT(genpos yypos, genpos (yypos+ 1)));
<INITIAL>">="    => (Tokens.GEQUAL(genpos yypos, genpos (yypos + 2)));
<INITIAL>"<="    => (Tokens.LEQUAL(genpos yypos, genpos (yypos+ 2)));

<INITIAL>"("   => (pushLexState PAREN_STATE;
		   Tokens.LPAREN(genpos yypos, genpos (yypos + 1)));
<INITIAL>")"   => (case lexState () of
		       SOME PAREN_STATE
		       => (popLexState ();
			   (case lexState () of
				SOME STRING_STATE => YYBEGIN STRING
			      | _ => ());
			   Tokens.RPAREN(genpos yypos, genpos (yypos + 1)))
		     | SOME s
		       => (error (genpos yypos) ($("Illegal state "^(lexStateToString s)^" at RPAREN"));
			   continue ())
		     | NONE
		       => (error (genpos yypos) ($"Illegal nil state stack at RPAREN");
			   continue ()));

<INITIAL>"["   => (pushLexState BRACE_STATE;
		   Tokens.LBRACE(genpos yypos, genpos (yypos + 1)));
<INITIAL>"]"   => (case lexState () of
		       SOME BRACE_STATE
		       => (popLexState ();
			   Tokens.RBRACE(genpos yypos, genpos (yypos + 1)))
		     | SOME s
		       => (error (genpos yypos) ($("Illegal state "^(lexStateToString s)^" at RBRACE"));
			   continue ())
		     | NONE
		       => (error (genpos yypos) ($"Illegal nil state stack at RBRACE");
			   continue ()));


<INITIAL>"{"   => (pushLexState CBRACE_STATE;
		   Tokens.LCBRACE(genpos yypos, genpos (yypos + 1)));
<INITIAL>"}"   => (case lexState () of
		       SOME CBRACE_STATE
		       => (popLexState ();
			   (case lexState () of
				SOME STRING_STATE => YYBEGIN STRING
			      | _ => ());
			   Tokens.RCBRACE(genpos yypos, genpos (yypos + 1)))
		     | SOME s
		       => (error (genpos yypos) ($("Illegal state "^(lexStateToString s)^" at RCBRACE"));
			   continue ())
		     | NONE
		       => (error (genpos yypos) ($"Illegal nil state stack at RCBRACE");
			   continue ()));

<INITIAL>"^"   => (Tokens.CARROT(genpos yypos, genpos (yypos+1)));

<INITIAL>".."   => (Tokens.ELLIPSIS(genpos yypos, genpos (yypos+2)));


<INITIAL>":"   => (Tokens.COLON(genpos yypos, genpos (yypos+1)));
<INITIAL>"="   => (Tokens.ASSIGN(genpos yypos, genpos (yypos+1)));

<INITIAL>"=>"  => (Tokens.EQARROW(genpos yypos, genpos (yypos + 2)));

<INITIAL>"->"  => (Tokens.ARROW(genpos yypos, genpos (yypos + 2)));
<INITIAL>"_"   => (Tokens.UNDERSCORE(genpos yypos, genpos (yypos + 1)));


<INITIAL>{INT} => ((* Rule to match integers. *)
		   let
                       val intval = (Int.fromString(yytext))
		   in
		       case intval of
			   SOME(a:int) => Tokens.INT(a, genpos yypos, genpos (yypos + (size yytext)))
			 | NONE => (error (genpos yypos) ($"Integer number conversion error");
				    continue())
		   end);

<INITIAL>{HEX} => ((* Rule to match hexadecimal numbers *)
		   let
		       val text = vetHex (removeUnderbars (String.extract (yytext, 2, NONE)))
			   handle IllegalChar c 
				  => "" before (error (genpos yypos) ($("Illegal character '" ^ (Char.toString c) ^ "' in hexadecimal")))
		       val intinfval = StringCvt.scanString (IntInf.scan (StringCvt.HEX)) text
		   in
		       case intinfval of
			   SOME a => Tokens.HEX (((size text) * 4 , a), genpos yypos, genpos (yypos + (size yytext)))
			 | NONE => (error (genpos yypos) ($"Hexadecimal number conversion error");
				    continue())
		   end);

<INITIAL>{OCT} => ((* Rule to match octal numbers *)
		   let
		       val text = vetOct (removeUnderbars (String.extract (yytext, 2, NONE)))
			   handle IllegalChar c 
				  => "" before (error (genpos yypos) ($("Illegal character '" ^ (Char.toString c) ^ "' in octal")))
		       val intinfval = StringCvt.scanString (IntInf.scan (StringCvt.OCT)) text
		   in
		       case intinfval of
			   SOME a => Tokens.OCT (((size text) * 3 , a), genpos yypos, genpos (yypos + (size yytext)))
			 | NONE => (error (genpos yypos) ($"Octal number conversion error");
				    continue())
		   end);

<INITIAL>{BIN} => ((* Rule to match binary numbers *)
		   let
		       val text = vetBin (removeUnderbars (String.extract (yytext, 2, NONE)))
			   handle IllegalChar c 
				  => "" before (error (genpos yypos) ($("Illegal character '" ^ (Char.toString c) ^ "' in binary")))
		       val intinfval = StringCvt.scanString (IntInf.scan (StringCvt.BIN)) text
		   in
		       case intinfval of
			   SOME a => Tokens.BIN ((size text, a), genpos yypos, genpos (yypos + (size yytext)))
			 | NONE => (error (genpos yypos) ($"Binary number conversion error");
				    continue())
		   end);


<INITIAL>{REAL} => ((* Rule to match real numbers. *)
		   let
                       val realval = (Real.fromString(yytext))
		   in
		       case realval of
			   SOME(a:real) => Tokens.REAL(a, genpos yypos, genpos (yypos + (size yytext)))
		    | NONE => (error (genpos yypos) ($"Real number conversion error");
			       continue())
		   end);

<INITIAL>"\"" => (YYBEGIN STRING;
		  pushLexState (STRING_STATE);
		  Tokens.BEGIN_STRING(genpos yypos, genpos (1 + yypos)));
 
<STRING>"\"" => (case lexState () of
		     SOME STRING_STATE
		     => (YYBEGIN INITIAL;
			 popLexState ();
			 Tokens.END_STRING (genpos yypos, genpos (1 + yypos)))
		   | SOME s
		     => (error (genpos yypos) ($("Illegal state "^(lexStateToString s)^" at END_STRING"));
			 continue ())
		   | NONE
		     => (error (genpos yypos) ($"Illegal nil state stack at END_STRING");
			 continue ()));
 
<STRING>"\$" => (YYBEGIN STRING_EXP;
		 Tokens.STRING_EXP(genpos yypos, genpos (1 + yypos)));
 
<STRING_EXP>{ID} => (YYBEGIN STRING;
		     Tokens.ID(yytext, genpos yypos, genpos (yypos + (size yytext))));

<STRING_EXP>"("   => (YYBEGIN INITIAL;
		      pushLexState PAREN_STATE;
		      Tokens.LPAREN(genpos yypos, genpos (yypos + 1)));
			  
<STRING_EXP>"{"   => (YYBEGIN INITIAL;
		      pushLexState CBRACE_STATE;
		      Tokens.LCBRACE(genpos yypos, genpos (yypos + 1)));
			  

<STRING>"\\"{EOL} => (ParserSettings.nextLine ();
		      lastline_yypos := yypos;
		      Tokens.STRING_CHARS(yytext, genpos yypos, genpos ((size yytext) + yypos)));

<STRING>{STRING_CHAR}+ => (Tokens.STRING_CHARS(yytext, genpos yypos, genpos ((size yytext) + yypos)));
 


<INITIAL>{ID}    => (Tokens.ID(yytext, genpos yypos, genpos (yypos + (size yytext))));





<INITIAL>{PATTERN} => ((* Rule to match patterns *)
		      let
			val s = substring (yytext, 1, (size yytext) - 2)
		      in
  		        Tokens.PATTERN (s, genpos yypos, genpos (yypos + (size s)))
		      end);


		      <INITIAL>{ID}    => (Tokens.ID(yytext, genpos yypos, genpos (yypos + (size yytext))));

<INITIAL>{EOL} => ((* Rule to match new line. *)
		     ParserSettings.nextLine ();
		     lastline_yypos := yypos;
		     (* Does not produce a newline token if the lexer state indicates we are inside parens. *)
		     case lexState ()
		      of NONE => Tokens.NEWLINE(genpos yypos, genpos (yypos))
		       | _ => continue());

<INITIAL>{WS}+ => ((* Rule to match whitespace. *)
		   continue());

<INITIAL>"//".*{EOL}  =>  (ParserSettings.nextLine ();
			   lastline_yypos := yypos;
			   (* Does not produce a newline token if the lexer state indicates we are inside parens. *)
			   case lexState ()
			    of NONE => Tokens.NEWLINE(genpos yypos, genpos (yypos))
			     | _ => continue());

<INITIAL>"/*"             =>  (YYBEGIN(COMMENT); 
			       continue());

<COMMENT>[^*\r\n]*               =>  (continue ());

<COMMENT>"*"+[^*/\r\n]*          =>  (continue ());

<COMMENT>{EOL}               =>  (ParserSettings.nextLine ();
				  lastline_yypos := yypos; 
				  continue());

<COMMENT>"*"+"/"                 =>  (YYBEGIN(INITIAL); 
				      continue ());

<COMMENT>.        => ((* Any unmatched characters in comments are ignored *)
		      continue ());

<INITIAL>.        => ((* Any unmatched characters produce an error. *)
		      (if yytext = ";" then
			   error (genpos yypos) ($("No semicolons are allowed in the Diesel language"))
		       else
			   error (genpos yypos) ($("Invalid character(s) encountered: '" ^ yytext ^ "'")));
		      continue());
