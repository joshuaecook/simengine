structure Printer =
struct

datatype text = SUB of text list | $ of string

fun text2str ($(str)) = 
    str
  | text2str (SUB(progs)) =
    String.concatWith "\n" (map text2str progs)

fun printtext (outstream, text, i) =
    let
	val indent_factor = 2

	fun say str = TextIO.output(outstream, str)		      
	    
	fun sayln str = (say str; say "\n")

	fun ind n =
	    let 
		fun ind' (n) = 
		    if n <= 0 then () else (say " "; ind' (n-1))
	    in
		ind' (n)
	    end
	    
	fun indent n = 
	    ind(n * indent_factor)

    in
	case text of 
	    SUB texts => app (fn(text) => printtext (outstream, text, i+1)) texts
	  | $ line => (indent i; sayln line)
    end
    before TextIO.flushOut(outstream)
(*    handle e => 
	   (DynException.log ("Printer.printtext") e;
	    raise e)*)

fun printtexts (outstream, texts, i) =
    app (fn(t) => printtext (outstream, t, i)) texts


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
