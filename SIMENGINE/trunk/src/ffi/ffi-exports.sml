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

(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

(* Exports heap allocation functions for all types of arrays returned
 * from external libraries. *)
structure FFIExports = struct

local
    structure Ptr = MLton.Pointer
    val theString: string ref = ref ""
    val theStrings: string list ref = ref nil
in

fun getTheString () = ! theString

fun makeTheString (size: int, ptr: Ptr.t) =
    theString := Vector.tabulate (size, (fn i => (chr o Word8.toInt o Ptr.getWord8) (ptr, i)))

fun pushAString (size: int, ptr: Ptr.t) =
    let val str = Vector.tabulate (size, (fn i => (chr o Word8.toInt o Ptr.getWord8) (ptr, i)))
    in
	theStrings := str :: (! theStrings)
    end

fun popAString () =
    case ! theStrings
     of str :: strs => SOME str before theStrings := strs
      | _ => NONE

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

val _ = _export "push_a_string": (int * MLton.Pointer.t -> unit) -> unit;
    pushAString



end (* structure FFIExports *)



