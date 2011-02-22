signature PRINTER = 
sig

type text = Layout.t
val $ : string -> text
val SUB : text list -> text
(*datatype text = SUB of text list | $ of string*)

val printtext : (IOUtil.outstream * text * int) -> unit
val printtexts : (IOUtil.outstream * text list * int) -> unit
val progs2file : (text list * string) -> unit
val printLayout : Layout.t -> IOUtil.outstream -> unit
val text2str : text -> string

end
structure Printer : PRINTER =
struct

type text = Layout.t

local
open Layout
in
fun text2layout text = text

fun $ string = 
    str string
fun SUB texts = 
    indent (align texts, 2)

end


fun text2str text =
    Layout.toString (text2layout text)

fun printLayout layout outstream =
    Layout.outputWidth (layout, 120, outstream)

fun printtexts (outstream, texts, i) = 
    let 
	val l = Layout.align (map text2layout texts)
	val l' = Layout.indent (l, i*2)
    in 
	printLayout (Layout.add_newline l') outstream
    end

fun printtext (outstream, text, i) = 
    let 
	val l = text2layout text
	val l' = Layout.indent (l, i*2)
    in 
	printLayout (Layout.add_newline l') outstream
    end


	
fun prog2file (text, filename) = 
    let
	val outputstream = TextIO.openOut filename
	val _ = printtext (outputstream, text, 0)
	val _ = TextIO.closeOut outputstream
    in
	()
    end

fun progs2file (texts, filename) = 
    let
	val outputstream = TextIO.openOut filename
	val _ = printtexts (outputstream, texts, 0)
	val _ = TextIO.closeOut outputstream
    in
	()
    end




fun withOpenIn filename proc =
    let val instream = TextIO.openIn filename
    in
	proc instream before TextIO.closeIn instream
	handle e => (TextIO.closeIn instream; raise e)
    end

fun withOpenOut filename proc =
    let val outstream = TextIO.openOut filename
    in
	proc outstream before TextIO.closeOut outstream
	handle e => (TextIO.closeOut outstream; raise e)
    end

fun withOpenInOut (infilename, outfilename) proc =
    withOpenIn infilename (fn (instream) => withOpenOut outfilename (fn (outstream) => proc (instream, outstream)))

end
