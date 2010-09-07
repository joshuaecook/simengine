(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)

structure Size: SIZE = struct


structure IntInfX = struct
open IntInf
val one = fromInt 1
val zero = fromInt 0
val equals = (fn EQUAL => true | _ => false) o compare
val isZero = fn x => equals (zero, x)
val toIntInf = fn x => x
val fromIntInf = fn x => x
end

type bits = IntInfX.int
type bytes = IntInfX.int
type vector = IntInfX.int




structure Vector = struct
open IntInfX
type t = vector
type bits = bits
type bytes = bytes

val bits = fn {size,scale} => size * scale
val bytes = fn {size,scale} => size * scale

fun byteAlign {size,scale,align} =
    let val size = bytes {size= size, scale= scale}
	val size = size + (align - 1) in
	size - (rem (size, align))
    end

fun isByteAligned {size,scale,align} = 
    0 = rem (bytes {size= size, scale= scale}, align)
end (* structure Vector *)



structure Bits = struct
type t = bits
type bytes = bytes
open IntInfX

val byte = fromInt 8

fun align {size,align} =
    let val size = size + (align - 1) in
	size - (rem (size, align))
    end

fun isAligned {size,align} = 0 = rem (size, align)

fun bytes n =
    quot (n, byte) + (if isAligned {size= n, align= byte} then 0 else 1)

structure Const = struct
    datatype t
      = Byte | Word8 | Word16 | Word32 | Word64 | Word128
    val toBits
      = fn Byte => byte
	 | Word8 => 8
	 | Word16 => 16
	 | Word32 => 32
	 | Word64 => 64
	 | Word128 => 128
    fun fromBits bits
      = case toInt bits
	 of 8 => Word8
	  | 16 => Word16
	  | 32 => Word32
	  | 64 => Word64
	  | 128 => Word128
	  | _ => raise Domain

    val compare =
     fn (a,b) => compare (toBits a, toBits b)
end

end (* structure Bits *)

structure Bytes = struct
type t = bytes
type bits = bits
open IntInfX

val bits = fn x => Bits.byte * x

fun align {size,align} =
    let val size = size + (align - 1) in
	size - (rem (size, align))
    end
fun isAligned {size,align} = 0 = rem (size, align)
end (* structure Bytes *)



local open Vector in
val pair = fromInt 2
val triple = pair + one
val quad = pair * pair
val cudaHalfWarp = fromInt 16
val cudaWarp = cudaHalfWarp * pair
end

local open Bits in
val byte = fromInt 8
val word8 = fromInt 8
val word16 = byte * pair
val word32 = word16 * pair
val word64 = word32 * pair
val word128 = word64 * pair
end

local open Bytes in
val byte4 = fromInt 4
val byte8 = byte4 * pair
val byte16 = byte8 * pair
val byte32 = byte16 * pair
val byte64 = byte32 * pair
val byte128 = byte64 * pair
end


end
