(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

(* Exports heap allocation functions for all types of arrays returned
 * from external libraries. *)
structure FFIExports = struct

local
    structure Ptr = MLton.Pointer
    val theString: string ref = ref ""
in

fun getTheString () = ! theString

fun makeTheString (size: int, ptr: Ptr.t) =
    theString := Vector.tabulate (size, (fn i => (chr o Word8.toInt o Ptr.getWord8) (ptr, i)))

end

val _ = _export "heap_alloc_word8": (int * Word8.word -> Word8.word array) -> unit;
    Array.array
val _ = _export "heap_update_word8": (Word8.word array * int * Word8.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_word32": (int * Word32.word -> Word32.word array) -> unit;
    Array.array
val _ = _export "heap_update_word32": (Word32.word array * int * Word32.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_word64": (int * Word64.word -> Word64.word array) -> unit;
    Array.array
val _ = _export "heap_update_word64": (Word64.word array * int * Word64.word -> unit) -> unit;
    Array.update

val _ = _export "heap_alloc_pointer": (int * MLton.Pointer.t -> MLton.Pointer.t array) -> unit;
    Array.array
val _ = _export "heap_update_pointer": (MLton.Pointer.t array * int * MLton.Pointer.t -> unit) -> unit;
    Array.update

val _ = _export "make_the_string": (int * MLton.Pointer.t -> unit) -> unit;
    makeTheString



end (* structure FFIExports *)



