(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
structure ImportHook =
struct

(* When an import hook is defined, it is called with the pathname of each imported file. *)

type importHook = string -> unit
val importHook: importHook option ref = ref NONE

(* Temporarily defines an import hook for a given thunk. *)
fun withImportHook hook f =
    let val old = ! importHook before importHook := SOME hook
    in
	f () before importHook := old
	handle e => (importHook := old; raise e)
    end


end
