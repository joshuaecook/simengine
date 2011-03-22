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

structure CType =
struct

datatype size
  = bit
  | bits32
  | bits64
  | decimal
  | arbitrary

datatype base
  = Boolean
  | Integer
  | Real
  | Complex

type numeric = (base * size)

datatype matrix
  = Dense
  | Banded
  | Tree
  | LAPACK of LAPACK.t

datatype container 
  = Scalar
  | SSEVector
  | Array of int
  | Matrix of ((int * int) * matrix)

type t = (container * base * size)

fun typeToContainer (c, _, _) = c
fun typeToBase      (_, b, _) = b
fun typeToSize      (_, _, s) = s
fun toType {container, base, size} = (container, base, size)

(* few helpers *)
fun directCSupport (b, s) =
    (case b of
	 Real => true
       | Integer => true
       | _ => false)
    andalso
    (case s of
	 bits32 => true
       | bits64 => true
       | _ => false)

local
    fun commensurate_sizes (size1, size2) = 
	case (size1, size2) of
	    (arbitrary, _) => arbitrary
	  | (_, arbitrary) => arbitrary
	  | (decimal, _) => decimal
	  | (_, decimal) => decimal
	  | (bits64, _) => bits64
	  | (_, bits64) => bits64
	  | (bits32, _) => bits32
	  | (_, bits32) => bits32
	  | (bit, _) => bit
       (* | (_, bit) => bit *) (* redundant *)

    fun commensurate_bases (base1, base2) =
	case (base1, base2) of
	    (Complex, _) => Complex
	  | (_, Complex) => Complex
	  | (Real, _) => Real
	  | (_, Real) => Real
	  | (Integer, _) => Integer
	  | (_, Integer) => Integer
	  | (Boolean, _) => Boolean
       (* | (_, Boolean) => Boolean *) (* redundant *)


    fun assignNumericToType ((c, _, _), {base, size}) = (c, base, size)

in

fun commensuratePair (t1, t2) =
    let
	val b = commensurate_bases (typeToBase t1, typeToBase t2)
	val s = commensurate_sizes (typeToSize t1, typeToSize t2)
    in
	(assignNumericToType (t1, {base=b, size=s}),
	 assignNumericToType (t2, {base=b, size=s}))
    end

fun commensurateTypes [] = []
  | commensurateTypes [one_type] = [one_type]
  | commensurateTypes [type1, type2] = 
    let
	val (t1, t2) = commensuratePair (type1, type2)
    in
	[t1, t2]
    end
  | commensurateTypes (type1::type2::rest) = 
    let
	val (first_type, second_type) = commensuratePair (type1, type2)
    in
	(* first and second types must be equivalent *)
	first_type::(commensurateTypes (first_type::rest))
    end
end


end
