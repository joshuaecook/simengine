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

functor SimexTest (S: SIMEX) = struct
fun bug why = raise Fail why

structure A = S.API

val simengine = S.new "fn.sim"

val api = S.api simengine
val _ = case A.version api
	 of 0 => ()
	  | i => bug ("Expected version to be 0 but got " ^ (Int.toString i))

val _ = let val iterators = A.iteratorNames api
	in case Vector.length iterators
	    of 0 => bug ("Expected a non-empty vector of iterator names")
	     | 1 => print ("Iterator " ^ (Vector.sub (iterators, 0)) ^ "\n")
	     | n =>
	       let val names = List.rev (Vector.foldl op:: nil iterators)
	       in print ("Iterators " ^ (String.concatWith ", " names))
	       end
	end

val _ = let val inputs = A.inputNames api
	    val numInputs = Vector.length inputs
	    val defaults = A.defaultInputs api
	in if numInputs <> Vector.length defaults
	   then bug ("Vectors of paramter names (" ^ (Int.toString numInputs) ^ ") and default parameters (" ^ (Int.toString (Vector.length defaults)) ^ ")  must be of similar length")
	   else
	       case Vector.length defaults
		of 0 => bug ("Expected a non-empty vector of default parameters")
		 | 1 => print ("Default parameter value " ^ (Vector.sub (inputs, 0)) ^ "=" ^ (Real64.toString (Vector.sub (defaults, 0))) ^ "\n")
		 | n => 
		   let val values = List.rev (Vector.foldl op:: nil defaults)
		       val names = List.rev (Vector.foldl op:: nil inputs)
		   in print ("Default parameter values " ^ (String.concatWith ", " (ListPair.map (fn (k,v) => k ^ " = " ^ (Real64.toString v)) (names, values))) ^ "\n")
		   end
	end

val _ = let val defaults = A.defaultStates api
	in case Vector.length defaults
	    of 0 => bug ("Expected a non-empty vector of default states")
	     | 1 => print ("Default state value " ^ (Real64.toString (Vector.sub (defaults, 0))) ^ "\n")
	     | n => 
	       let val values = List.rev (Vector.foldl op:: nil defaults)
	       in print ("Default state values [" ^ (String.concatWith "," (map Real64.toString values)) ^ "]\n")
	       end
	end

val _ = let val outputs = A.outputNames api
	    val numOutputs = Vector.length outputs
	    val numQuants = A.outputNumQuantities api
	in if numOutputs <> Vector.length numQuants
	   then bug ("Vectors of output names (" ^ (Int.toString numOutputs) ^ ") and output quantities (" ^ (Int.toString (Vector.length numQuants)) ^ ")  must be of similar length")
	   else let fun pair (k,v) = k ^ " = " ^ (Int32.toString v)
		in case Vector.length numQuants
		    of 0 => bug ("Expected a non-empty vector of output quantities")
		     | 1 => print ("Output quantity " ^ (pair (Vector.sub (outputs, 0),
							       Vector.sub (numQuants, 0))) ^ "\n")
		     | n =>
		       let val values = List.rev (Vector.foldl op:: nil numQuants)
			   val names = List.rev (Vector.foldl op:: nil outputs)
		       in print ("Outputs quantities " ^ (String.concatWith ", " (ListPair.map pair (names, values))) ^ "\n")
		       end
		end
	end

val _ = case A.hashcode api
	 of i => print ("Hashcode is " ^ (Int64.toString i) ^ "\n")

val _ = case A.numModels api
	 of 1 => ()
	  | i => bug ("Expected 1 model but got " ^ (Int.toString i))


val _ = let val solvers = A.solverNames api
	in case Vector.length solvers
	    of 0 => bug ("Expected a non-empty vector of solver names")
	     | 1 => print ("Solver " ^ (Vector.sub (solvers, 0)) ^ "\n")
	     | n =>
	       let val names = List.rev (Vector.foldl op:: nil solvers)
	       in print ("Solvers " ^ (String.concatWith ", " names))
	       end
	end

val _ = case A.target api
	 of s => print ("Targetting " ^ s ^ "\n")

val _ = case A.precision api
	 of A.Double => print ("Double precision\n")
	  | A.Single => print ("Single precision\n")

val _ = case A.name api
	 of "fn" => ()
	  | s => bug ("Expected name to be fn but got " ^ s)

val _ = S.release simengine

end


structure Test = SimexTest(Simex)
