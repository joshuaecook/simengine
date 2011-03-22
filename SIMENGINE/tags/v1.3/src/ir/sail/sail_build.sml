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

signature SAILBUILD =
sig

(* Create some common types *)
type proper_t
type t
val int32 : proper_t
val int64 : proper_t
val real32 : proper_t
val real64 : proper_t
val string : proper_t

(* Create compound types *)
val int32_array : proper_t
val int64_array : proper_t
val real32_array : proper_t
val real64_array : proper_t

(* Create basic constructs *)
type var_t
type binding_t
type atom_t
val sailvar : (string * proper_t) -> var_t
val sailval : ((string * proper_t) * atom_t) -> binding_t

(*fun expToSail : Exp.exp -> Sail.expression*)

end

structure SailBuild : SAILBUILD =
struct

(* start off with the types *)
datatype t = datatype Type.t
datatype proper_t = datatype Type.proper_t
local 
    open Type
in
val int32 = int 32
val real32 = real 32
val int64 = int 64
val real64 = real 64
val string = string

(* now add the compound types *)
val int32_array = apply(array, int32)
val int64_array = apply(array, int64)
val real32_array = apply(array, real32)
val real64_array = apply(array, real64)

(* internal commands *)
val gen = gen (* convert proper from a general type *)
end

(* basic constructs *)
local
    open Sail
in
datatype var_t = datatype TypeApplication.t
datatype binding_t = datatype Binding.t
datatype atom_t = datatype Atom.t
fun sailvar (x, t) =
    let val a = (Atom.Variable x)
    in TypeApplication.TypeApply {var=a, args=Vector.fromList [gen t]}
    end

(* this performs the following function:
 * val x : t = a
 *)
fun sailval ((x, t), a) = 
    Binding.Value {var=(x,gen t), object=a}
end

(*fun expToSail (TERM t) = termToSail t
and termToSailAtom (RATIONAL (n,d)) = *)

end
