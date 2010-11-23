(* http://rogerprice.org/ug/ug.pdf *)

structure Cxt = Context
val point = Cxt.point
val endpoint = fn yypos => fn yytext => point (yypos + (String.size yytext))

structure Tokens = Tokens
structure Operator = Syntax.Operator

structure Keywords = struct
val tab
  = [("program", Tokens.PROGRAM), 
     ("call", Tokens.CALL), 
     ("jump", Tokens.JUMP),
     ("switch", Tokens.SWITCH),
     ("return", Tokens.RETURN),
     ("default", Tokens.DEFAULT),
     ("halt", Tokens.HALT)]

fun find yytext =
    case List.find (fn (name,_) => name = yytext) tab
     of SOME (name,cons) => SOME (fn yypos => cons (point yypos, endpoint yypos yytext))
      | NONE => NONE
end

structure TypeKeywords = struct
val tab
  = [("int", Tokens.TYPE_INT),
     ("real", Tokens.TYPE_REAL),
     ("bool", Tokens.TYPE_BOOL),
     ("string", Tokens.TYPE_STRING),
     ("unit", Tokens.TYPE_UNIT)]
fun find yytext =
    case List.find (fn (name,_) => name = yytext) tab
     of SOME (name,cons) => SOME (fn yypos => cons (point yypos, endpoint yypos yytext))
      | NONE => NONE
end

structure CTypeKeywords = struct
val tab
  = [("struct", Tokens.C_STRUCT),
     ("union", Tokens.C_UNION),
     ("enum", Tokens.C_ENUM),
     ("const", Tokens.C_CONST),
     ("volatile", Tokens.C_VOLATILE)]
val types
  = ["void","char","short","int","long","float","double","signed","unsigned"]

fun find yytext =
    case List.find (fn (name,_) => name = yytext) tab
     of SOME (name,cons) => SOME (fn yypos => cons (point yypos, endpoint yypos yytext))
      | NONE => 
	case List.find (fn name => name = yytext) types
	 of SOME name => SOME (fn yypos => Tokens.C_TYPENAME (yytext, point yypos, endpoint yypos yytext))
	  | NONE => NONE
end

structure Punctuation = struct
val tab
  = [("{", Tokens.LBRACE),
     ("}", Tokens.RBRACE),
     ("[", Tokens.LSQUARE),
     ("]", Tokens.RSQUARE),
     ("(", Tokens.LPAREN),
     (")", Tokens.RPAREN),
     (":", Tokens.COLON),
     (",", Tokens.COMMA),
     ("=", Tokens.EQUALS),
     ("->", Tokens.ARROW),
     ("=>", Tokens.DARROW),
     ("*", Tokens.STAR),
     ("|", Tokens.VERTICAL),
     ("'''", Tokens.QQQAPOS),
     ("...", Tokens.ELLIPSIS),
     (";", Tokens.SEMICOLON)]
fun find yytext =
    case List.find (fn (name,_) => name = yytext) tab
     of SOME (name,cons) => SOME (fn yypos => cons (point yypos, endpoint yypos yytext))
      | NONE => NONE
end

type svalue = Tokens.svalue
type pos = Cxt.point
type ('a, 'b) token = ('a, 'b) Tokens.token
type arg = Cxt.context
type lexresult = (svalue, pos) token

fun eof context = Tokens.EOF (Cxt.point_at_bol (), endpoint 0 "")

fun invalid (message, context, text, point, endpoint) =
    let 
	val line = 1 + (Cxt.lines ())
	val column = Cxt.column point
	val endcol = Cxt.column endpoint
    in
	TextIO.output (
	TextIO.stdErr,
	message ^ " from line "^(Int.toString line)^" columns "^(Int.toString column)^"-"^(Int.toString endcol)^" of "^(Cxt.filename context)^"\n")
    end
		  
%%
%header (functor MakeSpilLexer(structure Tokens: SpilGrammar_TOKENS structure Context: PARSER_CONTEXT structure Syntax: SPIL));
%arg (context);
%s SPIL TYPE CTYPE;

eol = "\013\010" | "\010" | "\013";
quote = "\034";
digit = [0-9];
letter = [a-zA-Z];
ident_head = {letter} | "!" | "%" | "&" | "*" | "/" | "<" | ">" | "?" | "~" | "_" | "^";
ident_tail = {ident_head} | {digit} | "+" | "-";
ident = {ident_head} {ident_tail}*;
punct = "{" | "}" | "(" | ")" | "[" | "]" | ":" | "," | "=" | "->" | "=>" | ";" | "'''" | "..." | "*" | "|";
		     
%%

[\ \t]+ => (continue ());

{eol} => (Cxt.newline yypos; continue ());

{punct} => (case Punctuation.find yytext
	     of SOME cons => cons yypos
	      | NONE => raise Fail ("unrecognized punctuation "^yytext));

<INITIAL>[\ \t]* => (YYBEGIN SPIL;
		     Cxt.reset ();
		     continue ());

<SPIL>{ident} => (case Keywords.find yytext
		   of SOME cons => cons yypos
		    | NONE => 
		      case Operator.find yytext
		       of SOME oper => Tokens.OP (yytext, point yypos, endpoint yypos yytext)
			| NONE => 
			  Tokens.ID (yytext, point yypos, endpoint yypos yytext));

<SPIL>"yes" | "no" => (Tokens.BOOL (yytext, point yypos, endpoint yypos yytext));

<SPIL>"-"? {digit}+ => (Tokens.INT (yytext, point yypos, endpoint yypos yytext));

<SPIL>"-"? {digit}+ "." {digit}+ => (Tokens.REAL (yytext, point yypos, endpoint yypos yytext));

<SPIL>"-"? {digit}+ ("." {digit}+)? "e" {digit}+ => (Tokens.REAL (yytext, point yypos, endpoint yypos yytext));



<SPIL>"\"" => (YYBEGIN TYPE; Tokens.QTYPE (point yypos, endpoint yypos yytext));
<TYPE>"\"" => (YYBEGIN SPIL; Tokens.QUOTE (point yypos, endpoint yypos yytext));

<TYPE>{ident} => (case TypeKeywords.find yytext
		   of SOME cons => cons yypos
		    | NONE => Tokens.ID (yytext, point yypos, endpoint yypos yytext));

<TYPE>"-"? {digit}+ => (Tokens.INT (yytext, point yypos, endpoint yypos yytext));



<SPIL>"C\"" => (YYBEGIN CTYPE; Tokens.QCTYPE (point yypos, endpoint yypos yytext));
<CTYPE>"\"" => (YYBEGIN SPIL; Tokens.QUOTE (point yypos, endpoint yypos yytext));

<CTYPE>({letter} | "_") ({letter} | {digit} | "_")* => (case CTypeKeywords.find yytext
							 of SOME cons => cons yypos
							  | NONE => Tokens.C_ID (yytext, point yypos, endpoint yypos yytext));





<TYPE>. => (invalid ("Invalid type",context, yytext, point yypos, endpoint yypos yytext);
	    Tokens.INVALID (point yypos, endpoint yypos yytext));

<CTYPE>. => (invalid ("Invalid C type",context, yytext, point yypos, endpoint yypos yytext);
	     Tokens.INVALID (point yypos, endpoint yypos yytext));

<SPIL>. => (invalid ("Invalid character "^yytext,context, yytext, point yypos, endpoint yypos yytext);
	    continue ());



