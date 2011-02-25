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

signature CONTAINER =
sig

type matrix = Exp.exp Matrix.matrix
type array = Exp.exp Array.array

(* Boxing/Unboxing routines *)
val expArrayToArray : Exp.exp -> array 
val arrayToExpArray : array -> Exp.exp
val expMatrixToMatrix : Exp.exp -> matrix
val matrixToExpMatrix : matrix -> Exp.exp

(* Array/Matrix Creation *)
val zeros_array : int -> Exp.exp (* create an empty expression array *)
val zeros_matrix : (int * int) -> Exp.exp (* create a matrix of all zeros - this is a mutable object *)
val identity_matrix : (int) -> Exp.exp (* creates an identity matrix - again, a mutable object *)

(* Matrix operations over expressions *)
val transpose : Exp.exp -> Exp.exp (* transpose a matrix, creates a new matrix of the transpose.  Matrix.transpose changes the original matrix *)

(* Container operations *)
val containerToElements : Exp.container -> Exp.exp list

(* Array mappings *)
val arrayToList : array -> Exp.exp list
val listToArray : Exp.exp list -> array
val arrayToSize : array -> int

(* Matrix mappings *)
val expListToArrayList : Exp.exp list -> array list

end

structure Container:CONTAINER =
struct

type matrix = Exp.exp Matrix.matrix
type array = Exp.exp Array.array

val i2s = Util.i2s

fun vector2list v = map 
			(fn(i)=>Vector.sub (v,i)) 
			(List.tabulate (Vector.length v, (fn(x)=>x)))
fun arrayToList v = map 
			(fn(i)=>Array.sub (v,i)) 
			(List.tabulate (Array.length v, (fn(x)=>x)))
fun list2vector l = Vector.fromList l
fun listToArray l = Array.fromList l


fun vector2array v = listToArray (vector2list v)

fun arrayToSize a = Array.length a

fun expListToArrayList l =
    let
	fun exp2array (Exp.CONTAINER (Exp.ARRAY a)) = a
	  | exp2array _ = 
	    DynException.stdException("Unexpected expression",
				      "Container.expListToArrayList.exp2array",
				      Logger.INTERNAL)
    in
	map exp2array l
    end

fun containerToElements (Exp.EXPLIST l) = l
  | containerToElements (Exp.ARRAY a) = arrayToList a
  | containerToElements (Exp.ASSOC t) = SymbolTable.listItems t
  | containerToElements (Exp.MATRIX m) = Matrix.getElements m

val zero = Exp.TERM (Exp.INT 0)
val one = Exp.TERM (Exp.INT 1)

fun matrixToExpMatrix m =
    Exp.CONTAINER (Exp.MATRIX m)

fun arrayToExpArray a =
    Exp.CONTAINER (Exp.ARRAY a)

fun zeros_matrix (rows,cols) =
    matrixToExpMatrix (Matrix.zeros (Exp.calculus ()) (rows, cols))

fun zeros_array cols =
    arrayToExpArray (Array.array (cols, zero))

fun expArrayToArray (Exp.CONTAINER (Exp.ARRAY a)) = a
  | expArrayToArray _ = DynException.stdException("Non-array passed in", 
						  "Container.expArrayToArray", 
						  Logger.INTERNAL)



fun expMatrixToMatrix (Exp.CONTAINER (Exp.MATRIX m)) = m
  | expMatrixToMatrix _ = DynException.stdException("Non-matrix passed in", 
						   "Container.expMatrixToMatrix", 
						   Logger.INTERNAL)

fun identity_matrix (size) = 
    matrixToExpMatrix (Matrix.identity (Exp.calculus ()) size)


fun transpose exp =
    let
	val m = expMatrixToMatrix exp
	val m' = Matrix.clone m
	val _ = Matrix.transpose m'
    in
	matrixToExpMatrix m'
    end


end

