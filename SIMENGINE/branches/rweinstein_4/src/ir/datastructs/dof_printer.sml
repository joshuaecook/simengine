structure DOFPrinter =
struct 

val i2s = Util.i2s
val r2s = Util.r2s

fun eq2str (eq:DOF.eq) = 
    let

	fun eq2exp eq : Exp.exp = 
	    let
		val {eq_type, sourcepos, lhs, rhs} = eq
	    in
		ExpBuild.equals (Exp.TERM lhs, rhs)
	    end
	    
	val {eq_type,...} = eq
	val expstr = ExpProcess.exp2str (eq2exp eq)

	fun offset2str (sym, int) = 
	    (Symbol.name sym) ^ ":" ^ (i2s int)

	fun offsetlist2str offsets = 
	    String.concatWith "," (map offset2str offsets)
    in
	case eq_type
	 of DOF.INSTANCE {name, classname, offset} => 
	    "Instance " ^ (Symbol.name name) ^ " of " ^ (Symbol.name classname) ^ " @("^(offsetlist2str offset)^"): " ^ expstr
	  | DOF.DIFFERENCE_EQ {offset} => 
	    "Difference Equation @"^(i2s offset)^": " ^ expstr
	  | DOF.DERIVATIVE_EQ {offset} => 
	    "Derivative Equation @"^(i2s offset)^": " ^ expstr
	  | DOF.INTERMEDIATE_EQ => 
	    "Intermediate Equation: " ^ expstr
	  | DOF.INITIAL_VALUE {offset} => 
	    "Initial Value Equation @"^(i2s offset)^": " ^ expstr
    end

fun log_eqs (header, eqs) = 
    let
	val log = Util.log
    in
	(log "";
	 log header;
	 log ("--------------------------------------");
	 (app (fn(e)=>log (eq2str e)) eqs);
	 log ("--------------------------------------"))
    end

fun printModel (model: DOF.model) =
    let
	val _ = CurrentModel.setCurrentModel model

	val (classes, topinstance, systemproperties) = model

	fun header2str() =
	    let 
		val _ = Util.log ("")
		val _ = Util.log ("Top Level Model: " ^ (Symbol.name (#classname topinstance)))
		val _ = Util.log ("Class List: {"^(String.concatWith ", " (map (fn{name,...}=> Symbol.name name) classes)^"}"))
		val _ = Util.log ("")
	    in
		()
	    end

	fun contents2str [content] = 
	    ExpProcess.exp2str content
	  | contents2str contents =
	    "(" ^ (String.concatWith ", " (map ExpProcess.exp2str contents)) ^ ")"

	fun printClass (class as {name, properties={sourcepos}, inputs, outputs, exps, eqs}) =
	     (print ("Class Name: " ^ (Symbol.name (name)) ^ "\n");
	      print ("  Inputs: " ^ (String.concatWith ", " (map (fn{name,default} => ExpProcess.exp2str (Exp.TERM name) ^ (case default of SOME v => (" = "^(ExpProcess.exp2str v)) | NONE => "")) (!inputs))) ^ "\n");
	      print ("  Equations orig:\n");
	      app (fn(e) => print("    " ^ (eq2str e) ^ "\n")) (!eqs);
	      print ("  Equations new:\n");
	      app (fn(e) => print("    " ^ (ExpProcess.exp2str e) ^ "\n")) (!exps);
	      print ("  Outputs: " ^ (String.concatWith ", " (map (fn({name, contents, condition}) => (ExpProcess.exp2str (Exp.TERM name)) ^ " = " ^ (contents2str contents) ^ " when " ^ (ExpProcess.exp2str condition)) 
								  (!outputs))) ^ "\n");
	      print ("  Symbols: {"^(String.concatWith ", " (map Symbol.name (ClassProcess.findSymbols class)))^"}\n"))
	    

	fun printTopInstance ({name, classname}) =
	    let
		val class = CurrentModel.classname2class classname
	    in
		(print ("  Name: " ^ (case name of SOME name => Symbol.name name | NONE => "NONE") ^ "\n");
		 print ("  Class: " ^ (Symbol.name classname) ^ "\n");
		 print ("  Inputs: " ^ (String.concatWith ", " (map (fn{name,...} => ExpProcess.exp2str (Exp.TERM name)) (!(#inputs class)))) ^ "\n");
		 print ("  Outputs: " ^ (String.concatWith ", " (map (fn({name, contents, condition}) => ExpProcess.exp2str (Exp.TERM name)) (!(#outputs class)))) ^ "\n"))
	    end

	fun printSystemProperties {iterators,time,precision} =
	    (print (" time interval: ["^(r2s (#1 time))^","^(r2s (#2 time))^"]\n");
	     print (" precision: "^(case precision of DOF.SINGLE => "single" | DOF.DOUBLE => "float")^"\n");
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
			    | Solver.CVODE {dt, abs_tolerance, rel_tolerance} =>
			      print ("  Solver = CVode (dt = )" ^ (Real.toString dt) ^ ", abs_tolerance = " ^ (Real.toString abs_tolerance) ^", rel_tolerance = " ^ (Real.toString rel_tolerance) ^ ")\n"))
		       | DOF.DISCRETE => 
			 print ("  Discrete\n"))
		 )
		 iterators)

    in
	if DynamoOptions.isFlagSet "logdof" then
	    (header2str();	 
	     print("CLASSES:\n");
	     app printClass classes;
	     print("\n\n");
	     print("TOP LEVEL INSTANCE:\n");
	     printTopInstance topinstance;
	     print("\n");
	     printSystemProperties systemproperties)
	else
	    ()
    end

(*

	val (classes, (topclass, topinst), globalproperties, id2instance) = model

	fun printClass class =
	    let
		val {name, properties, quants, submodels, outputs, instances} = class

(*		fun tunable2str {name, dimensions, initialval, properties} =
		    name ^ (case dimensions of 
				nil => "=" ^ (Real.toString initialval)
			      | dims => "[" ^ (String.concatWith ", " (map Int.toString dimensions)) ^ "]")*)


(*		fun name2str n = 
		    let
			fun nameComponent2str (NAME s) = s
			  | nameComponent2str (INDEX i) = "[" ^ (Int.toString i) ^ "]"
		    in
			String.concatWith "." (map nameComponent2str n)
		    end*)

		fun exp2str (SYMBOL (QUANTITY n, p)) = (*name2str n*) Symbol.name n
		  | exp2str (SYMBOL (SUBMODEL_OUTPUT {submodel, output}, p)) = (case UniqueTable.look (id2instance, submodel) of
										    SOME instanceref => (Symbol.name (#instanceName (!instanceref)))
										  | NONE => "UnknownID")
									       ^ "." ^ (Symbol.name output)
		  | exp2str (REAL r) = Real.toString r
		  | exp2str (BOOLEAN b) = Bool.toString b
		  | exp2str (OP {name, args}) = (Symbol.name name) ^ "(" ^ (String.concatWith ", " (map exp2str args)) ^ ")"
		    

		fun repeat string 0 = ""
		  | repeat string times = string ^ (repeat string (times-1))

		fun eq2str (name, DIFFERENCE {offset, exp}) = (Symbol.name name) ^ "[" ^ (Int.toString offset) ^ "] = " ^ (exp2str exp)
		  | eq2str (name, DIFFERENTIAL {order, exp}) = (Symbol.name name) ^ (repeat "'" order) ^ " = " ^ (exp2str exp)
		  | eq2str (name, INTERMEDIATE_EQ exp) = (Symbol.name name) ^ " = " ^ (exp2str exp)

		fun q2str {name, initialValue, dimensions, properties, equation} =
		    eq2str (name, equation)

		fun instanceCount (instance: instance ref) =
		    foldl (op * ) 1 (#dimensions (!instance))

		val _ = print ("CLASS: " ^ (case name of SOME n => (Symbol.name n) | NONE => "NONE") ^ "\n")
(*		val _ = print ("Tunables: \n\t" ^ (String.concatWith "\n\t" (map tunable2str tunables)) ^ "\n")*)
		val _ = print ("Equations: \n\t" ^ (String.concatWith "\n\t" (map q2str (!quants))) ^ "\n")
		val _ = print ("Instances: " ^ (Int.toString(foldl (op +) 0 (map instanceCount (!instances)))) ^ " total models in " ^ (Int.toString (length (!instances))) ^ " instances\n")
		val _ = print ("=====================================\n\n\n")
	    in
		()
	    end
    in
	app printClass classes
    end
*)

end
