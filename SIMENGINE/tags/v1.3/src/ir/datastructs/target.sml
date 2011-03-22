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

(* Data structure for target specific information *)
structure Target =
struct

(* TODO encode more metadata in this datatype. *)
datatype target 
  = CPU (* generic C target *)
  | OPENMP (* using the underlying OpenMP libraries *)
  | CUDA (*of {arch: cudaArchitecture,
	     deviceId: int,
	     emulate: bool}*)

and cudaArchitecture 
  = COMPUTE_10
  | COMPUTE_11
  | COMPUTE_12
  | COMPUTE_13
  | COMPUTE_20


(* of {compute: computecapability, (* right now, this can be 1.1 or 1.3 *)
	     multiprocessors: int, (* number of multiprocessors *)
	     globalMemory: int (* amount of global memory *)} *)

(* for CUDA, there are two compute capabilities that are supported, 1.1 and 1.3 *)
(* and computecapability = COMPUTE11 | COMPUTE14 *)

(* debugging commands *)
val i2s = Util.i2s
fun target2str t =
    case t 
     of CPU => "CPU"
      | OPENMP => "OPENMP"
      | CUDA => "CUDA" (*{compute=COMPUTE11, multiprocessors, globalMemory} => "CUDA {compute capability: 1.1, # of multi-processors: "^(i2s multiprocessors)^", global memory (KB): "^(i2s globalMemory)^"}"
      | CUDA {compute=COMPUTE13, multiprocessors, globalMemory} => "CUDA {compute capability: 1.3, # of multi-processors: "^(i2s multiprocessors)^", global memory (KB): "^(i2s globalMemory)^"}" *)

end
