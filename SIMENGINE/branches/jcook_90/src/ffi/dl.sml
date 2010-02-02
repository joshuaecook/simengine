(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
 * See http://mlton.org/CallingFromSMLToCFunctionPointer *)
signature DYNAMIC_LIBRARY = sig
    (* A handle for a dynmically-loaded library. *)
    type library 
    (* Specifies options for importing. *)
    type mode
    val RTLD_LAZY: mode
    val RTLD_NOW: mode

    (* An external function pointer. *)
    type function
    (* An external symbol pointer. *)
    type symbol     

    val new: string * mode -> library
    val release: library -> unit
    val function: library * string -> function
    val symbol: library * string -> symbol
end

structure DL:> DYNAMIC_LIBRARY = struct
datatype library = LIB of {filename: string,
			   dylib: MLton.Pointer.t}
type mode = Word32.word
type function = MLton.Pointer.t
type symbol = MLton.Pointer.t

val RTLD_LAZY = 0wx00001 (* Lazy function call binding.  *)
val RTLD_NOW  = 0wx00002 (* Immediate function call binding.  *)

(* Nb, strings passed as parameters must be null-terminated. *)
val dlopen = _import "dlopen": string * mode -> MLton.Pointer.t;
val dlerror = _import "dlerror": unit -> MLton.Pointer.t;
val dlclose = _import "dlclose": MLton.Pointer.t -> Int32.int;
val dlsym = _import "dlsym": MLton.Pointer.t * string -> MLton.Pointer.t;

fun cstring name = name ^ (str (chr (Word8.toInt 0w0)))

fun cstringToString address =
    (* Reads one byte at a time until a null byte is encoutered. *)
    let fun loop (ptrdiff, chars) =
	    case Byte.byteToChar (MLton.Pointer.getWord8 (address, ptrdiff))
	     of #"\000" => implode (rev chars)
	      | c => loop (1 + ptrdiff, c :: chars)
    in
	loop (0, nil)
    end

fun error () =
    let val charptr = dlerror ()
    in if MLton.Pointer.null = charptr
       then NONE
       else SOME (cstringToString charptr)
    end

fun release (LIB {filename, dylib}) =
    case dlclose dylib
     of 0 => ()
      | _ => raise Fail (case error () of SOME e => e | NONE => ("Unknown error attempting to close " ^ filename))


fun symbol (LIB {dylib, ...}, name) =
    let val sym = dlsym (dylib, cstring name)
    in case error ()
	of NONE => sym
	 | SOME e => raise Fail e
    end

val function = symbol

fun new (filename, mode) =
    let val dylib = dlopen (cstring filename, mode)
    in if MLton.Pointer.null = dylib
       then raise Fail (case error () of SOME e => e | NONE => ("Unknown error attempting to open " ^ filename))
       else LIB {filename = filename, dylib = dylib}
    end

end
