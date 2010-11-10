signature SOLVERPROCESS =
sig

    (* Given a name and a table of settings, return the matching solver structure *)
    val name2solver : Symbol.symbol * (Symbol.symbol * Exp.exp) list -> Solver.solver


end
structure SolverProcess : SOLVERPROCESS =
struct

local
    open Solver
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
	case has settings "cv_solv" of
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
	  | "auto" => AUTO {dt=getDT settings}
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

end
