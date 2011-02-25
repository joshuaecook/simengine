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

signature SAILUTIL =
sig

val print_task : Sail.Abstraction.t -> unit
val print_sml_task : Sail.Abstraction.t -> unit
val print_sail : Sail.Program.t -> unit

end

structure SailUtil : SAILUTIL =
struct

local
    open Layout
    fun printLayout layout outstream =
	output (layout, outstream)

    fun read_library () = 
	let
	    val ins = TextIO.openIn "library.sml"
	    fun helper layout copt = 
		case copt of
		    NONE => (TextIO.closeIn ins; layout)
		  | SOME c => helper (seq [layout, str c]) (TextIO.inputLine ins)
	in
	    seq [helper empty (TextIO.inputLine ins),
		 str ";"]
	end

    fun log_layout l = 
	let
	    val setup = align [read_library(),
			       newline,
			       seq [l, str ";"],
			       newline]
	in
	    printLayout setup TextIO.stdOut
	end
	
in
fun print_sail s = ()
fun print_task t = 
    log_layout (Sail.Abstraction.toLayout t)

fun print_sml_task t =
    log_layout (Sail.Abstraction.toSML t)

end



end
