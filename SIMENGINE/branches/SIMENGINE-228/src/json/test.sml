signature TEST = sig
val bug: string -> 'a
val pass: unit -> unit
end

structure Test: TEST = struct
fun bug why = raise Fail why
fun pass () = print "."
end
