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

signature DOFPRINTER =
sig
    val printModel : DOF.model -> unit
    val printClass : DOF.class -> unit

    (* additional output code*)
    val outputToStr : DOF.Output.output -> string
end
structure DOFPrinter : DOFPRINTER =
struct 

fun printModel model =
    Util.log_layout (DOFLayout.model_to_layout model)
					       
fun printClass class = 
    Util.log_layout (DOFLayout.class_to_layout class)

fun outputToStr output = 
    Layout.toString (DOFLayout.output_to_layout output)

(*

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str

fun genlist2str data2str [data] = 
    data2str data
  | genlist2str data2str datalist =
    "(" ^ (String.concatWith ", " (map data2str datalist)) ^ ")"

val symbollist2str = genlist2str Symbol.name
val contents2str = genlist2str e2s

fun printClass (class as {name, properties={sourcepos, preshardname, classform}, inputs, outputs, exps}) =
    (print ("Class Name: " ^ (Symbol.name (name)) ^ "\n");
     (case classform of
	  DOF.INSTANTIATION {readstates, writestates} => 
	  print (" |-> States read: "^ (symbollist2str readstates) ^ ", States written: " ^(symbollist2str writestates)^ "\n"));
     print(" |-> Class cost: " ^ (i2s (Cost.class2cost class)) ^ "\n");
     print ("  Inputs: " ^ 
	    (String.concatWith ", " (map (fn input => e2s (Exp.TERM (DOF.Input.name input)) ^ 
						      (case DOF.Input.default input of SOME v => (" = "^(e2s v)) | NONE => "")) (!inputs))) ^ "\n");
     print ("  Equations:\n");
     app (fn(e) => 
	    let
		val size = (*(ExpProcess.exp2size iterators e)
			   handle _ => *)(~1)

		val cost = (Cost.exp2cost e)
			   handle _ => (~1)
		val prefix = "  (x" ^ (i2s size) ^ ", cost="^(i2s cost)^") "
	    in
		if ExpProcess.isInstanceEq e orelse ExpProcess.isOutputEq e then
		    let
			val {classname, instname, inpargs, outargs, props} = ExpProcess.deconstructInst e
		    in
			print(prefix ^ "[" ^ (String.concatWith ", " (map Symbol.name (#iterators props))) ^ "]: " ^ (e2s e) ^ "\n")
		    end
		else
		    print(prefix ^ (e2s e) ^ "\n")
	    end
	 ) (!exps);
     print ("  Outputs:\n" ^ 
	    (String.concatWith "\n" (map (fn(out) => "    " ^ (outputToStr out))
					 (!outputs))) ^ "\n");
     print ("  Symbols: {"^(String.concatWith ", " (SymbolSet.toStrList (ClassProcess.findSymbols class)))^"}\n"))
    handle e => DynException.checkpoint "DOFPrinter.printClass" e

and outputToStr output = e2s (Exp.TERM (DOF.Output.name output)) ^
			 " = " ^ (contents2str (DOF.Output.contents output)) ^ 
			 " when " ^ (e2s (DOF.Output.condition output))    


fun printModel (model: DOF.model) =
    let
	val prevModel = CurrentModel.getCurrentModel()
	val _ = CurrentModel.setCurrentModel model

	val (classes, topinstance, systemproperties) = model

	fun getClass (classname) = 
	    SOME (CurrentModel.classname2class classname)
	    handle DynException.NoClassFound _ => NONE
		       
	val num_states = case getClass (#classname topinstance) of 
			      SOME class => ClassProcess.class2statesize class
			    | NONE => ~1

	fun header2str() =
	    let 
		val _ = Util.log ("")
		val _ = Util.log ("Top Level Model: " ^ (Symbol.name (#classname topinstance)))
		val _ = Util.log ("Class List: {"^(String.concatWith ", " (map (fn{name,...}=> Symbol.name name) classes)^"}"))
		val _ = Util.log ("Iterator list: {"^(String.concatWith ", " (map (fn(itersym,_)=>Symbol.name itersym) (#iterators systemproperties))) ^ "}")
	    in
		()
	    end


	fun printTopInstance ({name, classname}) =
	    let
		val class = getClass(classname)
			    
	    in
		case class of
		    SOME class =>
		    (print ("  Name: " ^ (case name of SOME name => Symbol.name name | NONE => "NONE") ^ "\n");
		     print ("  Class: " ^ (Symbol.name classname) ^ "\n");
		     print ("  Inputs: " ^ (String.concatWith ", " (map (e2s o Exp.TERM o DOF.Input.name) (!(#inputs class)))) ^ "\n");
		     print ("  Outputs: " ^ (String.concatWith ", " (map (e2s o Exp.TERM o DOF.Output.name) (!(#outputs class)))) ^ "\n");
		     print ("  Cost: "^ (i2s (Cost.class2cost class)) ^"\n");
		     print ("  State Count: "^(i2s num_states)^"\n"))
		  | NONE =>
		    (print ("  Name: " ^ (case name of SOME name => Symbol.name name | NONE => "NONE") ^ "\n");
		     print ("  Class: " ^ (Symbol.name classname) ^ "\n");
		     print ("  NO CLASS FOUND WITH THIS NAME\n"))
	    end

	fun printSystemProperties {iterators,precision,target,parallel_models,debug,profile} =
	    (print (" precision: "^(case precision of DOF.SINGLE => "float" | DOF.DOUBLE => "double")^"\n");
	     print (" target: "^(Target.target2str target)^"\n");
	     print (" number of parallel models: "^(i2s parallel_models)^"\n");
	     (if debug then print (" DEBUG mode enabled\n") else ());
	     (if profile then print (" PROFILE mode enabled\n") else ());
	     app
		 (fn(sym, itertype)=>
		    (print (" iterator: " ^ (Symbol.name sym) ^ "\n");
		     case itertype of
			 DOF.CONTINUOUS solver => 
			 (case solver of
			      Solver.FORWARD_EULER {dt} =>
			      print ("  Solver = Forward Euler (dt = " ^ (Real.toString dt) ^ ")\n")
			    | Solver.EXPONENTIAL_EULER {dt} =>
			      print ("  Solver = Exponential Euler (dt = " ^ (Real.toString dt) ^ ")\n")
			    | Solver.LINEAR_BACKWARD_EULER {dt, solv} =>
			      print ("  Solver = Linear Backward Euler (dt = " ^ (Real.toString dt) ^ 
				     (case solv of 
					  Solver.LSOLVER_DENSE => " Dense linear solver"
					| Solver.LSOLVER_BANDED {lowerhalfbw, upperhalfbw} => " Banded linear solver with "^(i2s lowerhalfbw)^" lower and "^(i2s upperhalfbw)^" upper bands")
				     ^")\n")
			    | Solver.RK4 {dt} =>
			      print ("  Solver = RK4 (dt = " ^ (Real.toString dt) ^ ")\n")
			    | Solver.MIDPOINT {dt} =>
			      print ("  Solver = Midpoint Method (dt = " ^ (Real.toString dt) ^ ")\n")
			    | Solver.HEUN {dt} =>
			      print ("  Solver = Heun (dt = " ^ (Real.toString dt) ^ ")\n")
			    | Solver.ODE23 {dt, abs_tolerance, rel_tolerance} =>
			      print ("  Solver = ODE23 (dt = " ^ (Real.toString dt) ^ ", abs_tolerance = " ^ (Real.toString abs_tolerance) ^", rel_tolerance = " ^ (Real.toString rel_tolerance) ^ ")\n")
			    | Solver.ODE45 {dt, abs_tolerance, rel_tolerance} =>
			      print ("  Solver = ODE45 (dt = " ^ (Real.toString dt) ^ ", abs_tolerance = " ^ (Real.toString abs_tolerance) ^", rel_tolerance = " ^ (Real.toString rel_tolerance) ^ ")\n")
			    | Solver.CVODE {dt, abs_tolerance, rel_tolerance,lmm,iter,solv,max_order} =>
			      print ("  Solver = CVode (dt = " ^ (Real.toString dt) ^ ", abs_tolerance = " ^ (Real.toString abs_tolerance) ^", rel_tolerance = " ^ (Real.toString rel_tolerance) ^ ", max_order = " ^ (i2s max_order) ^ ", lmm = "^(case lmm of Solver.CV_ADAMS => "CV_ADAMS" | Solver.CV_BDF => "CV_BDF")^", iter = "^(case iter of Solver.CV_NEWTON => "CV_NEWTON" | Solver.CV_FUNCTIONAL => "CV_FUNCTIONAL")^", solv = " ^ (case solv of Solver.CVDENSE => "CVDENSE" | Solver.CVDIAG => "CVDIAG" | Solver.CVBAND {upperhalfbw, lowerhalfbw} => "CVBAND("^(i2s lowerhalfbw)^","^(i2s upperhalfbw)^")") ^ ")\n")
			    | Solver.UNDEFINED => 
			      print ("  Solver = Undefined"))
		       | DOF.DISCRETE {sample_period} => 
			 print ("  Discrete with Ts="^(r2s sample_period)^", fs="^(r2s (1.0/sample_period))^"\n")
		       | DOF.ALGEBRAIC (DOF.PREPROCESS, iter) => 
			 print ("  Pre processing iterator of " ^ (Symbol.name iter) ^ "\n")
		       | DOF.ALGEBRAIC (DOF.INPROCESS, iter) => 
			 print ("  Inline processing iterator of " ^ (Symbol.name iter) ^ "\n")
		       | DOF.ALGEBRAIC (DOF.POSTPROCESS, iter) =>
			 print ("  Post processing iterator of " ^ (Symbol.name iter) ^ "\n")
		       | DOF.UPDATE iter =>
			 print ("  Updating iterator of " ^ (Symbol.name iter) ^ "\n")
		       | DOF.IMMEDIATE =>
			 print ("  Immediate iterator\n"))
		 )
		 iterators)

    in
	(if DynamoOptions.isFlagSet "logdof" then
	     (header2str();	 
	      print("CLASSES:\n");
	      app printClass classes;
	      print("\n\n");
	      print("TOP LEVEL INSTANCE:\n");
	      printTopInstance topinstance;
	      print("\n");
	      printSystemProperties systemproperties;
	      print("\n"))
	 else
	     ();
	 CurrentModel.setCurrentModel(prevModel))
    end
    handle e => DynException.checkpoint "DOFPrinter.printModel" e
*)
end
