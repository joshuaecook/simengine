(* Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C. *)
signature SIZE
  (* A library for describing sizes of objects in bits, bytes, and uniform vectors.
   * Sizes are arbitrarily large and provide basic algebraic properties
   * along with total ordering. A few common constant values and datatype
   * constants are defined.
   *)
  = sig
    type bits
    type bytes
    type vector

    (* Common constant values. *)
    val byte: bits
    val word8: bits
    val word16: bits
    val word32: bits
    val word64: bits
    val word128: bits

    val byte4: bytes
    val byte8: bytes
    val byte16: bytes
    val byte32: bytes
    val byte64: bytes
    val byte128: bytes

    val pair: vector
    val triple: vector
    val quad: vector
    val cudaHalfWarp: vector
    val cudaWarp: vector

    structure Bits: sig
	type t
	type bytes

	val bytes: t -> bytes

	val fromInt: int -> t
	val fromIntInf: IntInf.int -> t
	val toInt: t -> int (* may raise Overflow *)
	val toIntInf: t -> IntInf.int
	val compare: t * t -> order
	val + : t * t -> t
	val - : t * t -> t
	val ~ : t -> t
	val < : t * t -> bool
	val <= : t * t -> bool
	val > : t * t -> bool
	val >= : t * t -> bool
	val max: t * t -> t
	val min: t * t -> t
	val equals: t * t -> bool
	val isZero: t -> bool

	val one: t
	val zero: t

	val align: {size:t, align:t} -> t
	val isAligned: {size:t, align:t} -> bool				

	structure Const: sig
	    (* Datatype constants which can be used in pattern matching
	     * along with conversion of algebraic values. *)
	    datatype t
	      = Byte | Word8 | Word16 | Word32 | Word64 | Word128
	    val toBits: t -> bits
	    val fromBits: bits -> t (* may raise Domain *)

	    val compare: t * t -> order
	end
    end
    sharing type Bits.t = bits

    structure Bytes: sig
	type t
	type bits

	val bits: t -> bits

	val fromInt: int -> t
	val fromIntInf: IntInf.int -> t
	val toInt: t -> int (* may raise Overflow *)
	val toIntInf: t -> IntInf.int
	val compare: t * t -> order
	val + : t * t -> t
	val - : t * t -> t
	val ~ : t -> t
	val < : t * t -> bool
	val <= : t * t -> bool
	val > : t * t -> bool
	val >= : t * t -> bool
	val max: t * t -> t
	val min: t * t -> t
	val equals: t * t -> bool
	val isZero: t -> bool

	val one: t
	val zero: t

	val align: {size:t, align:t} -> t
	val isAligned: {size:t, align:t} -> bool				
    end
    sharing type Bytes.t = bytes
    sharing type Bytes.t = Bits.bytes
    sharing type Bytes.bits = Bits.t

    structure Vector: sig
	type t
	type bits
	type bytes

	val bits: {size:t, scale:bits} -> t
	val bytes: {size:t, scale:bytes} -> bytes

	val fromInt: int -> t
	val fromIntInf: IntInf.int -> t
	val toInt: t -> int (* may raise Overflow *)
	val toIntInf: t -> IntInf.int
	val compare: t * t -> order
	val + : t * t -> t
	val - : t * t -> t
	val ~ : t -> t
	val < : t * t -> bool
	val <= : t * t -> bool
	val > : t * t -> bool
	val >= : t * t -> bool
	val max: t * t -> t
	val min: t * t -> t
	val equals: t * t -> bool
	val isZero: t -> bool

	val one: t
	val zero: t

	val byteAlign: {size:t, scale:bytes, align:bytes} -> t
	val isByteAligned: {size:t, scale:bytes, align:bytes} -> bool
    end
    sharing type Vector.t = vector
    sharing type Vector.bits = Bits.t
    sharing type Vector.bytes = Bytes.t

end
