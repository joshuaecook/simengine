signature DOFLAYOUT =
sig
    type layout = Layout.t
    val model_to_layout : DOF.model -> layout
    val class_to_layout : DOF.class -> layout

    (* additional output code*)
    val output_to_layout : DOF.Output.output -> layout
end
structure DOFLayout : DOFLAYOUT =
struct 

type layout = Layout.t

val i2s = Util.i2s
val r2s = Util.r2s
val e2s = ExpPrinter.exp2str

(* Layout functions *)
open Layout
val s2l = str
val sym2l = s2l o Symbol.name
val i2l = s2l o i2s
val r2l = s2l o Real.toString
val e2l = ExpPrinter.exp2terselayout false
fun commas_seg t = seq (separate (t, ","))
fun commas_aligned t = mayAlign (separate (t, ","))


fun genlist2layout data2layout [data] = 
    data2layout data
  | genlist2layout data2layout datalist =
    parenList (map data2layout datalist)
    (*"(" ^ (String.concatWith ", " (map data2str datalist)) ^ ")"*)

val symbollist2layout = genlist2layout sym2l
val contents2layout = genlist2layout e2l

fun class_to_layout (class as {name, properties={sourcepos, preshardname, classform}, inputs, outputs, exps}) =
    align [label ("Class Name", sym2l name),
	   align ((case classform of 
		       DOF.INSTANTIATION {readstates, writestates} => 
		       [label ("States read", symbollist2layout readstates),
			label ("States written", symbollist2layout writestates)]) @
		  [label ("Class cost", i2l (Cost.class2cost class))]),
	   label ("Inputs", 
		  curlyList (map 
				 (fn input => seq [e2l (Exp.TERM (DOF.Input.name input)),
						   (case DOF.Input.default input of 
							SOME v => seq [s2l " = ", (e2l v)] 
						      | NONE => empty)]) 
				 (!inputs))),
	   heading ("Equations",
		    align (map equation_to_layout (!exps))),
	   heading ("Outputs",
		    align (map output_to_layout (!outputs))),
	   label ("Symbols", 
		  curlyList (map s2l (SymbolSet.toStrList (ClassProcess.findSymbols class))))]		
    handle e => DynException.checkpoint "DOFLayout.class_to_layout" e

and equation_to_layout e = 
    let
	val size = (*(ExpProcess.exp2size iterators e)
		    handle _ => *)(~1)

	val cost = (Cost.exp2cost e)
	    handle _ => (~1)

	val prefix = seq [parenList [label ("size", i2l size),
				     label ("cost", i2l cost)],
			  s2l " "]
    in
	if ExpProcess.isInstanceEq e orelse ExpProcess.isOutputEq e then
	    let
		val {classname, instname, inpargs, outargs, props} = ExpProcess.deconstructInst e
	    in
		mayAlign [prefix, 
			  indent (seq [bracketList (map sym2l (#iterators props)),
				       s2l ": ", 
				       e2l e], 2)]
	    end
	else
	    mayAlign [prefix,
		      indent (e2l e, 2)]
    end

and output_to_layout output = seq [e2l (Exp.TERM (DOF.Output.name output)),
				   s2l " = ",
				   contents2layout (DOF.Output.contents output),
				   s2l " when ",
				   e2l (DOF.Output.condition output)]

fun model_to_layout (model: DOF.model) =
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

	fun header_to_layout () =
	    align [label ("Top Level Model", sym2l (#classname topinstance)),
		   label ("Class List", curlyList (map (fn{name,...}=> sym2l name) classes))]
	    

	fun top_instance_to_layout ({name, classname}) =
	    let
		val class = getClass(classname)
			    
	    in
		case class of
		    SOME class =>
		    align [label ("Name", (case name of SOME name => sym2l name | NONE => s2l "NONE")),
			   label ("Class", sym2l classname),
			   label ("Inputs", curlyList (map (e2l o Exp.TERM o DOF.Input.name) (!(#inputs class)))),
			   label ("Outputs", curlyList (map (e2l o Exp.TERM o DOF.Output.name) (!(#outputs class)))),
			   label ("Cost", i2l (Cost.class2cost class)),
			   label ("State Count", i2l num_states)]
		  | NONE =>
		    align [label ("Name", (case name of SOME name => sym2l name | NONE => s2l "NONE")),
			   label ("Class", sym2l classname),
			   s2l "NO CLASS FOUND WITH THIS NAME"]
	    end
	    handle e => DynException.checkpoint "DOFLayout.model_to_layout.top_instance_to_layout" e

	fun system_properties_to_layout {iterators,precision,target,parallel_models,debug,profile} =
	    align [label ("precision", s2l (case precision of DOF.SINGLE => "float" | DOF.DOUBLE => "double")),
		   label ("target", s2l (Target.target2str target)),
		   label ("# of parallel models", i2l parallel_models),
		   if debug then s2l "DEBUG mode enabled" else empty,
		   if profile then s2l "PROFILE mode enabled" else empty,
		   heading ("iterators", 
			    align (map iterator_to_layout iterators))]

	and iterator_to_layout (sym, itertype) =
	    (label (Symbol.name sym,
		    case itertype of
			DOF.CONTINUOUS solver => 
			solver_to_layout solver
		      | DOF.DISCRETE {sample_period} => 
			s2l ("Discrete with Ts="^(r2s sample_period)^", fs="^(r2s (1.0/sample_period)))
		      | DOF.ALGEBRAIC (DOF.PREPROCESS, iter) => 
			s2l ("Pre processing iterator of " ^ (Symbol.name iter))
		      | DOF.ALGEBRAIC (DOF.INPROCESS, iter) => 
			s2l ("Inline processing iterator of " ^ (Symbol.name iter))
		      | DOF.ALGEBRAIC (DOF.POSTPROCESS, iter) =>
			s2l ("Post processing iterator of " ^ (Symbol.name iter))
		      | DOF.UPDATE iter =>
			s2l ("Updating iterator of " ^ (Symbol.name iter))
		      | DOF.IMMEDIATE =>
			s2l "Immediate iterator"))

	and solver_to_layout solver =
	    (case solver of
		 Solver.FORWARD_EULER {dt} =>
		 label ("solver", seq [s2l "Forward Euler ", curlyList [label ("dt", r2l dt)]])
	       | Solver.EXPONENTIAL_EULER {dt} =>
		 label ("solver", seq [s2l "Exponential Euler ", curlyList [label ("dt", r2l dt)]])
	       | Solver.LINEAR_BACKWARD_EULER {dt, solv} =>
		 label ("solver", 
			seq [s2l "Linear Backward Euler ", 
			     curlyList [label ("dt", r2l dt),
					label ("method", 
					       case solv of 
						   Solver.LSOLVER_DENSE => 
						   s2l "Dense linear solver"
						 | Solver.LSOLVER_BANDED {lowerhalfbw, upperhalfbw} =>
						   seq [s2l "Banded linear solver with ",
							i2l lowerhalfbw,
							s2l " lower and ",
							i2l upperhalfbw,
							s2l " upper bands"])]])
	       | Solver.RK4 {dt} =>
		 label ("solver", seq [s2l "RK4 ", curlyList [label ("dt", r2l dt)]])
	       | Solver.MIDPOINT {dt} =>
		 label ("solver", seq [s2l "Midpoint ", curlyList [label ("dt", r2l dt)]])
	       | Solver.HEUN {dt} =>
		 label ("solver", seq [s2l "Heun ", curlyList [label ("dt", r2l dt)]])
	       | Solver.ODE23 {dt, abs_tolerance, rel_tolerance} =>
		 label ("solver", seq [s2l "ODE23 ", curlyList [label ("dt", r2l dt),
								label ("abstol", r2l abs_tolerance),
								label ("reltol", r2l rel_tolerance)]])
	       | Solver.ODE45 {dt, abs_tolerance, rel_tolerance} =>
		 label ("solver", seq [s2l "ODE45 ", curlyList [label ("dt", r2l dt),
								label ("abstol", r2l abs_tolerance),
								label ("reltol", r2l rel_tolerance)]])
	       | Solver.CVODE {dt, abs_tolerance, rel_tolerance,lmm,iter,solv,max_order} =>
		 label ("solver", 
			seq [s2l "CVode ", 
			     curlyList 
				 [label ("dt", r2l dt),
				  label ("abstol", r2l abs_tolerance),
				  label ("reltol", r2l rel_tolerance),
				  label ("max_order", i2l max_order),
				  label ("lmm", s2l (case lmm of Solver.CV_ADAMS => "CV_ADAMS" 
							       | Solver.CV_BDF => "CV_BDF")),
				  label ("iter", s2l (case iter of Solver.CV_NEWTON => "CV_NEWTON" 
								 | Solver.CV_FUNCTIONAL => "CV_FUNCTIONAL")),
				  label ("solv", case solv of Solver.CVDENSE => s2l "CVDENSE" 
							    | Solver.CVDIAG => s2l "CVDIAG" 
							    | Solver.CVBAND {upperhalfbw, lowerhalfbw} =>
							      label ("CVBAND", parenList [i2l lowerhalfbw,
											  i2l upperhalfbw]))
		       ]])
	       | Solver.UNDEFINED => 
		 label ("solver", s2l "Undefined"))
	    handle e => DynException.checkpoint "DOFLayout.solver_to_layout" e

	val layout = if DynamoOptions.isFlagSet "logdof" then
			 align [heading ("Header", header_to_layout()),
				newline,
				heading ("Classes", align (map class_to_layout classes)),
				newline,
				heading ("Top Level", top_instance_to_layout topinstance),
				newline,
				heading ("System Properties", system_properties_to_layout systemproperties),
				newline, newline, newline]
		     else
			 empty
    in
	 (CurrentModel.setCurrentModel(prevModel);
	  layout)
    end
    handle e => DynException.checkpoint "DOFLayout.model_to_layout" e

end
