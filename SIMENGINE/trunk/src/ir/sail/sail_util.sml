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
