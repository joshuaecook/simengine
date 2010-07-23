signature SOLVER =
sig

    (* Solver data type *)
    datatype linear_solver =
	     LSOLVER_DENSE
	   | LSOLVER_BANDED of {upperhalfbw: int, lowerhalfbw: int}

    datatype cvode_lmm = CV_ADAMS | CV_BDF
    datatype cvode_iter = CV_NEWTON | CV_FUNCTIONAL

    datatype cvode_solver = 
	     CVDENSE
	   | CVDIAG
	   | CVBAND of {upperhalfbw:int, lowerhalfbw:int}

    datatype solver =
	     FORWARD_EULER of {dt:real}
	   | EXPONENTIAL_EULER of {dt:real}
	   | LINEAR_BACKWARD_EULER of {dt:real, solv: linear_solver}
	   | RK4 of {dt:real}
	   | MIDPOINT of {dt:real}
	   | HEUN of {dt:real}
	   | ODE23 of {dt:real, abs_tolerance: real, rel_tolerance: real}
	   | ODE45 of {dt:real, abs_tolerance: real, rel_tolerance: real}
	   | CVODE of {dt:real, abs_tolerance: real, rel_tolerance: real,
		       lmm: cvode_lmm, iter: cvode_iter, solv: cvode_solver,
		       max_order: int}
	   | UNDEFINED
		      
    (* Solver accessor methods *)
    val solver2name : solver -> string (* name of the solver *)
    val solver2shortname : solver -> string (* simpler, shorter name *)
    val solver2params : solver -> (string * string) list (* this function is used when generating generic solver properties in C *)
    val solver2opts : solver -> (string * string) list (* this function is used to generate solver specific options in C *)
    val solver2dt : solver -> real option (* this function returns an optional fixed timestep *)

    (* Given a name and a table of settings, return the matching solver structure *)
    val name2solver : Symbol.symbol * (Symbol.symbol * Exp.exp) list -> solver

    (* Solver predicates *)
    val isVariableStep : solver -> bool (* is this a variable time step solver *)

    (* Get the default solver if none was specified *)
    val default : solver

end
structure Solver : SOLVER =
struct

datatype linear_solver =
	 LSOLVER_DENSE
       | LSOLVER_BANDED of {upperhalfbw: int, lowerhalfbw: int}

datatype cvode_lmm = CV_ADAMS | CV_BDF
datatype cvode_iter = CV_NEWTON | CV_FUNCTIONAL

datatype cvode_solver = 
	 CVDENSE
       | CVDIAG
       | CVBAND of {upperhalfbw:int, lowerhalfbw:int}

datatype solver =
	 FORWARD_EULER of {dt:real}
       | EXPONENTIAL_EULER of {dt:real}
       | LINEAR_BACKWARD_EULER of {dt:real, solv: linear_solver}
       | RK4 of {dt:real}
       | MIDPOINT of {dt:real}
       | HEUN of {dt:real}
       | ODE23 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | ODE45 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | CVODE of {dt:real, abs_tolerance: real, rel_tolerance: real,
		   lmm: cvode_lmm, iter: cvode_iter, solv: cvode_solver,
		   max_order: int}
       | UNDEFINED

val i2s = Util.i2s
val r2s = Util.r2s

val default = ODE45 {dt=0.1, abs_tolerance=(1e~6), rel_tolerance=(1e~3)}

(* these are defined in solvers.c *)
fun solver2name (FORWARD_EULER _) = "forwardeuler"
  | solver2name (EXPONENTIAL_EULER _) = "exponentialeuler"
  | solver2name (LINEAR_BACKWARD_EULER _) = "linearbackwardeuler"
  | solver2name (RK4 _) = "rk4"
  | solver2name (MIDPOINT _) = "midpoint"
  | solver2name (HEUN _) = "heun"
  | solver2name (ODE23 _) = (*"ode23"*) "bogacki_shampine"
  | solver2name (ODE45 _) = (*"ode45"*) "dormand_prince"
  | solver2name (CVODE _) = "cvode"
  | solver2name (UNDEFINED) = "undefined"

(* these are defined in solvers.c *)
fun solver2shortname (FORWARD_EULER _) = "forwardeuler"
  | solver2shortname (EXPONENTIAL_EULER _) = "exponentialeuler"
  | solver2shortname (LINEAR_BACKWARD_EULER _) = "linearbackwardeuler"
  | solver2shortname (RK4 _) = "rk4"
  | solver2shortname (MIDPOINT _) = "midpoint"
  | solver2shortname (HEUN _) = "heun"
  | solver2shortname (ODE23 _) = "ode23" (*"bogacki_shampine"*)
  | solver2shortname (ODE45 _) = "ode45" (*"dormand_prince"*)
  | solver2shortname (CVODE _) = "cvode"
  | solver2shortname (UNDEFINED) = "undefined"

fun solver2params (FORWARD_EULER {dt}) = [("timestep", r2s dt),
					  ("abstol", "0.0"),
					  ("reltol", "0.0")]
  | solver2params (LINEAR_BACKWARD_EULER {dt, ...}) = [("timestep", r2s dt),
							    ("abstol", "0.0"),
							    ("reltol", "0.0")]
  | solver2params (EXPONENTIAL_EULER {dt}) = [("timestep", r2s dt),
					      ("abstol", "0.0"),
					      ("reltol", "0.0")]
  | solver2params (RK4 {dt}) = [("timestep", r2s dt),
				("abstol", "0.0"),
				("reltol", "0.0")]
  | solver2params (MIDPOINT {dt}) = [("timestep", r2s dt),
				     ("abstol", "0.0"),
				     ("reltol", "0.0")]
  | solver2params (HEUN {dt}) = [("timestep", r2s dt),
				 ("abstol", "0.0"),
				 ("reltol", "0.0")]
  | solver2params (ODE23 {dt, abs_tolerance, rel_tolerance}) = 
    [("timestep", r2s dt),
     ("abstol", r2s abs_tolerance),
     ("reltol", r2s rel_tolerance)]
  | solver2params (ODE45 {dt, abs_tolerance, rel_tolerance}) = 
    [("timestep", r2s dt),
     ("abstol", r2s abs_tolerance),
     ("reltol", r2s rel_tolerance)]
  | solver2params (CVODE {dt, abs_tolerance, rel_tolerance, ...}) = 
    [("timestep", r2s dt),
     ("abstol", r2s abs_tolerance),
     ("reltol", r2s rel_tolerance)]
  | solver2params (UNDEFINED) = []


fun linear_backward_euler_solver2opts LSOLVER_DENSE = [("lsolver", "LSOLVER_DENSE")]
  | linear_backward_euler_solver2opts (LSOLVER_BANDED {lowerhalfbw, upperhalfbw}) = [("lsolver", "LSOLVER_BANDED"),
										     ("upperhalfbw", i2s upperhalfbw),
										     ("lowerhalfbw", i2s lowerhalfbw)]

fun cvode_solver2opts CVDENSE = [("solv", "CVODE_DENSE")]
  | cvode_solver2opts CVDIAG = [("solv", "CVODE_DIAG")]
  | cvode_solver2opts (CVBAND {upperhalfbw, lowerhalfbw}) = 
    [("solv", "CVODE_BAND"),
     ("upperhalfbw", i2s upperhalfbw),
     ("lowerhalfbw", i2s lowerhalfbw)]

fun solver2opts (LINEAR_BACKWARD_EULER {dt, solv}) = linear_backward_euler_solver2opts solv
  | solver2opts (CVODE {dt, abs_tolerance, rel_tolerance, lmm, iter, solv, max_order}) =
    [("max_order", i2s max_order),
     ("lmm", case lmm of CV_ADAMS => "CV_ADAMS" | CV_BDF => "CV_BDF"),
     ("iter", case iter of CV_FUNCTIONAL => "CV_FUNCTIONAL" | CV_NEWTON => "CV_NEWTON")] @
    cvode_solver2opts solv
  | solver2opts _ =
    nil

fun isVariableStep (ODE23 _) = true
  | isVariableStep (ODE45 _) = true
  | isVariableStep (CVODE _) = true
  | isVariableStep _ = false

fun solver2dt (FORWARD_EULER {dt}) = SOME dt
  | solver2dt (EXPONENTIAL_EULER {dt}) = SOME dt
  | solver2dt (LINEAR_BACKWARD_EULER {dt,...}) = SOME dt
  | solver2dt (RK4 {dt}) = SOME dt
  | solver2dt (MIDPOINT {dt}) = SOME dt
  | solver2dt (HEUN {dt}) = SOME dt
  | solver2dt (ODE23 _) = NONE
  | solver2dt (ODE45 _) = NONE
  | solver2dt (CVODE {dt,...}) = if dt > 0.0 then
				     SOME dt
				 else
				     NONE
  | solver2dt UNDEFINED = NONE


local
    fun error s = (Logger.log_error (Printer.$("Unexpected error processing solver properties: " ^ s));
		   DynException.setErrored())
    fun has settings setting = List.find (fn(sym, exp)=>sym = (Symbol.symbol setting)) settings
    
    fun getDT settings =
    case has settings "dt" of
	SOME (_, Exp.TERM (Exp.REAL r)) => r
      | _ => 0.1

    fun getAbsTol settings =
	case has settings "abstol" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => r
	  | _ => 1e~6
    fun getRelTol settings =
	case has settings "reltol" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => r
	  | _ => 1e~3
    fun getCVMaxOrder settings =
	case has settings "cv_maxorder" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => Real.round r
	  | _ => 5
    fun getCVUpperHalfBW settings =
	case has settings "cv_upperhalfbw" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => Real.round r
	  | _ => 1
    fun getCVLowerHalfBW settings =
	case has settings "cv_upperlowerbw" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => Real.round r
	  | _ => 1
    fun getCVLMM settings =
	case has settings "cv_lmm" of
	    SOME (_, Exp.TERM (Exp.STRING s)) => 
	    (case s of
		 "CV_BDF" => CV_BDF
	       | "CV_ADAMS" => CV_ADAMS
	       | _ => (error ("Unknown linear multistep method '"^s^"'");
		       CV_BDF))
	  | _ => CV_BDF
    fun getCVIter settings =
	case has settings "cv_iter" of
	    SOME (_, Exp.TERM (Exp.STRING s)) => 
	    (case s of
		 "CV_NEWTON" => CV_NEWTON
	       | "CV_FUNCTIONAL" => CV_FUNCTIONAL
	       | _ => (error ("Unknown nonlinear solver iteration method '"^s^"'");
		       CV_NEWTON))
	  | _ => CV_NEWTON
    fun getCVSolv settings =
	case has settings "cv_iter" of
	    SOME (_, Exp.TERM (Exp.STRING s)) => 
	    (case s of
		 "CV_DENSE" => CVDENSE
	       | "CV_BAND" => CVDIAG
	       | "CV_DIAG" => CVBAND {upperhalfbw=getCVUpperHalfBW settings,
				      lowerhalfbw=getCVLowerHalfBW settings}
	       | _ => (error ("Unknown CVODE solver '"^s^"'");
		       CVDENSE))
	  | _ => CVDENSE
    fun getLBEUpperHalfBW settings =
	case has settings "lbe_upperhalfbw" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => Real.round r
	  | _ => 0
    fun getLBELowerHalfBW settings =
	case has settings "lbe_upperlowerbw" of
	    SOME (_, Exp.TERM (Exp.REAL r)) => Real.round r
	  | _ => 0
    fun getLBESolv settings =
	case has settings "lbe_solv" of
	    SOME (_, Exp.TERM (Exp.STRING s)) => 
	    (case s of
		 "LSOLVER_DENSE" => LSOLVER_DENSE
	       | "LSOLVER_BANDED" => LSOLVER_BANDED {upperhalfbw=getLBEUpperHalfBW settings,
						     lowerhalfbw=getLBELowerHalfBW settings}
	       | _ => (error ("Unknown linear solver '"^s^"'");
		      LSOLVER_DENSE))
	  | _ => LSOLVER_DENSE

in
fun name2solver (solver_sym, settings) =
    let
	val solver_name = Symbol.name solver_sym

(*	val forwardeuler = Symbol.symbol "forwardeuler"
	val exponentialeuler = Symbol.symbol "exponentialeuler"
	val linearbackwardeuler = Symbol.symbol "linearbackwardeuler"
	val rk4 = Symbol.symbol "rk4"
	val midpoint = *)
    in
	case solver_name of
	    "forwardeuler" => FORWARD_EULER {dt=getDT settings}
	  | "exponentialeuler" => EXPONENTIAL_EULER {dt=getDT settings}
	  | "linearbackwardeuler" => LINEAR_BACKWARD_EULER {dt=getDT settings,
							    solv=getLBESolv settings}
	  | "rk4" => RK4 {dt=getDT settings}
	  | "midpoint" => MIDPOINT {dt=getDT settings}
	  | "heun" => HEUN {dt=getDT settings}
	  | "ode23" => ODE23 {dt=getDT settings, 
			      abs_tolerance=getAbsTol settings, 
			      rel_tolerance=getRelTol settings}
	  | "ode45" => ODE45 {dt=getDT settings, 
			      abs_tolerance=getAbsTol settings, 
			      rel_tolerance=getRelTol settings}
	  | "cvode" => CVODE {dt=getDT settings, 
			      abs_tolerance=getAbsTol settings, 
			      rel_tolerance=getRelTol settings,
			      lmm=getCVLMM settings,
			      iter=getCVIter settings,
			      solv=getCVSolv settings,
			      max_order=getCVMaxOrder settings}
	  | "cvode_stiff" => CVODE {dt=getDT settings, 
				    abs_tolerance=getAbsTol settings, 
				    rel_tolerance=getRelTol settings,
				    lmm=CV_BDF,
				    iter=CV_NEWTON,
				    solv=getCVSolv settings,
				    max_order=getCVMaxOrder settings}
	  | "cvode_diag" => CVODE {dt=getDT settings, 
				   abs_tolerance=getAbsTol settings, 
				   rel_tolerance=getRelTol settings,
				   lmm=CV_BDF,
				   iter=CV_NEWTON,
				   solv=CVDIAG,
				   max_order=getCVMaxOrder settings}
	  | "cvode_tridiag" => CVODE {dt=getDT settings, 
				   abs_tolerance=getAbsTol settings, 
				   rel_tolerance=getRelTol settings,
				   lmm=CV_BDF,
				   iter=CV_NEWTON,
				   solv=CVBAND {upperhalfbw=1, lowerhalfbw=1},
				   max_order=getCVMaxOrder settings}
	  | "cvode_nonstiff" => CVODE {dt=getDT settings, 
				       abs_tolerance=getAbsTol settings, 
				       rel_tolerance=getRelTol settings,
				       lmm=CV_ADAMS,
				       iter=CV_FUNCTIONAL,
				       solv=getCVSolv settings,
				       max_order=12}
	  | _ => (error ("Undefined solver type: " ^ solver_name);
		  UNDEFINED)
    end

end
(*
fun solver2options solver = 
    let
	val param_list = solver2params solver
	fun getItem tag = 
	    case List.find (fn(a,b)=>a = tag) param_list
	     of SOME (a,b) => b
	      | NONE => DynException.stdException (("Can't get item '"^tag^"' from param list"), "Solver.solver2options.getItem", Logger.INTERNAL)
    in
	{dt=getItem "DT",
	 abstol=getItem "ABSTOL",
	 reltol=getItem "RELTOL"}
    end
fun solver2options (FORWARD_EULER {dt}) = 
    {dt=dt,
     abstol=0.0,
     reltol=0.0}
  | solver2options (EXPONENTIAL_EULER {dt}) = 
    {dt=dt,
     abstol=0.0,
     reltol=0.0}
  | solver2options (RK4 {dt}) = 
    {dt=dt,
     abstol=0.0,
     reltol=0.0}
  | solver2options (MIDPOINT {dt}) =
    {dt=dt,
     abstol=0.0,
     reltol=0.0}
  | solver2options (HEUN {dt}) = 
    {dt=dt,
     abstol=0.0,
     reltol=0.0}
  | solver2options (ODE23 {dt, abs_tolerance, rel_tolerance}) = 
    {dt=dt,
     abstol=abs_tolerance,
     reltol=abs_tolerance}
  | solver2options (ODE45 {dt, abs_tolerance, rel_tolerance}) = 
    {dt=dt,
     abstol=abs_tolerance,
     reltol=abs_tolerance}
  | solver2options (CVODE {dt, abs_tolerance, rel_tolerance}) = 
    {dt=dt,
     abstol=abs_tolerance,
     reltol=abs_tolerance}

fun solver2params solver = 
    let
	val {dt, abstol, reltol} = solver2options solver
    in
	[("DT", r2s dt),
	 ("ABSTOL", r2s abstol),
	 ("RELTOL", r2s reltol)]
    end
*)

end
