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

structure Printer =
struct

datatype text = SUB of text list | $ of string

fun text2str ($(str)) = 
    str
  | text2str (SUB(progs)) =
    String.concatWith "\n" (map text2str progs)

fun takeWhile f nil = nil
  | takeWhile f (x::xs) =
    if f x then x :: (takeWhile f xs) else takeWhile f xs


fun printLayout layout outstream =
    Layout.output (layout, outstream)



fun printtext (outstream, text, i) =
    let
	val indent_factor = 2

	fun say str = TextIO.output(outstream, str)		      
	    
	fun sayln str = (say str; say "\n"; TextIO.StreamIO.flushOut (TextIO.getOutstream outstream))

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
