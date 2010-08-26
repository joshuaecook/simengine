signature SAILUTIL =
sig

val print_task : Sail.Abstraction.t -> unit
val print_sail : Sail.Program.t -> unit

end

structure SailUtil : SAILUTIL =
struct

local
    fun printLayout layout outstream =
	Layout.output (layout, outstream)

    fun log_layout l = 
	printLayout l TextIO.stdOut
in
fun print_sail s = ()
fun print_task t = 
    let
	val l = Sail.Abstraction.toLayout t
	val l' = Layout.add_newline l
    in
	log_layout l'
    end
end

end
