functor SimexTest (S: SIMEX) = struct
fun bug why = raise Fail why

structure A = S.API
structure M = S.API.Metadata

val simengine = S.new "fn.sim"

val api = S.api simengine
val meta = A.metadata api
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

val _ = case M.hashcode meta
	 of i => print ("Hashcode is " ^ (Int64.toString i) ^ "\n")

val _ = case M.numModels meta
	 of 1 => ()
	  | i => bug ("Expected 1 model but got " ^ (Int.toString i))


val _ = let val solvers = M.solverNames meta
	in case Vector.length solvers
	    of 0 => bug ("Expected a non-empty vector of solver names")
	     | 1 => print ("Solver " ^ (Vector.sub (solvers, 0)) ^ "\n")
	     | n =>
	       let val names = List.rev (Vector.foldl op:: nil solvers)
	       in print ("Solvers " ^ (String.concatWith ", " names))
	       end
	end

val _ = case M.target meta
	 of s => print ("Targetting " ^ s ^ "\n")

val _ = case M.precision meta
	 of M.Double => print ("Double precision\n")
	  | M.Single => print ("Single precision\n")

val _ = case A.name api
	 of "fn" => ()
	  | s => bug ("Expected name to be fn but got " ^ s)

val _ = S.release simengine

end


structure Test = SimexTest(Simex)
