structure T = JSONToken
type lexresult = T.token

fun eof () = T.EOF

fun int s = T.INT (valOf (IntInf.fromString s))
fun real s = T.REAL (valOf (LargeReal.fromString s))

val buffer: string list ref = ref nil
fun addString s = buffer := s :: !buffer

fun string () = T.STRING (String.concat (List.rev (!buffer))) before buffer := nil

%%

%header (structure LexJSON);
%s STRING;

whitespace = [\ \t\n\t];
digit = [0-9];
int = "-"?({digit} | [1-9]{digit}+);
frac = "."{digit}+;
exp = [eE][-+]?{digit}+;
real = {int}{frac} | {int}{exp} | {int}{frac}{exp};
hex = {digit} | [a-fA-F];

%%

<INITIAL>{whitespace}+ => ( continue () );

<INITIAL>"{" => ( T.LOBJECT );
<INITIAL>"}" => ( T.ROBJECT );
<INITIAL>"[" => ( T.LARRAY );
<INITIAL>"]" => ( T.RARRAY );
<INITIAL>"," => ( T.COMMA );
<INITIAL>":" => ( T.COLON );
<INITIAL>"null" => ( T.NULL );
<INITIAL>"false" => ( T.FALSE );
<INITIAL>"true" => ( T.TRUE );

<INITIAL>{int} => ( int yytext );
<INITIAL>{real} => ( real yytext );

<INITIAL>"\"" => ( YYBEGIN STRING; continue () );
<STRING>"\"" => ( YYBEGIN INITIAL; string () );
<STRING>[^\"]+ => ( addString yytext; continue () );

<INITIAL>. => ( continue () );
