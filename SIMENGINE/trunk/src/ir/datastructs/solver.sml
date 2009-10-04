structure Solver =
struct

datatype cvode_lmm = CV_ADAMS | CV_BDF
datatype cvode_iter = CV_NEWTON | CV_FUNCTIONAL

datatype cvode_solver = 
	 CVDENSE
       | CVDIAG
       | CVBAND of {upperhalfbw:int, lowerhalfbw:int}

datatype solver =
	 FORWARD_EULER of {dt:real}
       | EXPONENTIAL_EULER of {dt:real}
       | RK4 of {dt:real}
       | MIDPOINT of {dt:real}
       | HEUN of {dt:real}
       | ODE23 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | ODE45 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | CVODE of {dt:real, abs_tolerance: real, rel_tolerance: real,
		   lmm: cvode_lmm, iter: cvode_iter, solv: cvode_solver}

val i2s = Util.i2s
val r2s = Util.r2s

(* these are defined in solvers.c *)
fun solver2name (FORWARD_EULER _) = "forwardeuler"
  | solver2name (EXPONENTIAL_EULER _) = "exponentialeuler"
  | solver2name (RK4 _) = "rk4"
  | solver2name (MIDPOINT _) = "midpoint"
  | solver2name (HEUN _) = "heun"
  | solver2name (ODE23 _) = (*"ode23"*) "bogacki_shampine"
  | solver2name (ODE45 _) = (*"ode45"*) "dormand_prince"
  | solver2name (CVODE _) = "cvode"

(* these are defined in solvers.c *)
fun solver2shortname (FORWARD_EULER _) = "forwardeuler"
  | solver2shortname (EXPONENTIAL_EULER _) = "exponentialeuler"
  | solver2shortname (RK4 _) = "rk4"
  | solver2shortname (MIDPOINT _) = "midpoint"
  | solver2shortname (HEUN _) = "heun"
  | solver2shortname (ODE23 _) = "ode23" (*"bogacki_shampine"*)
  | solver2shortname (ODE45 _) = "ode45" (*"dormand_prince"*)
  | solver2shortname (CVODE _) = "cvode"

fun cvode_solver2params CVDENSE = [("CVODE_SOLV", "CVODE_DENSE")]
  | cvode_solver2params CVDIAG = [("CVODE_SOLV", "CVODE_DIAG")]
  | cvode_solver2params (CVBAND {upperhalfbw, lowerhalfbw}) = 
    [("CVODE_SOLV", "CVODE_BAND"),
     ("CVODE_UPPERHALFBW", i2s upperhalfbw),
     ("CVODE_LOWERHALFBW", i2s lowerhalfbw)]

fun solver2params (FORWARD_EULER {dt}) = [("DT", r2s dt),
					  ("ABSTOL", "0.0"),
					  ("RELTOL", "0.0")]
  | solver2params (EXPONENTIAL_EULER {dt}) = [("DT", r2s dt),
					      ("ABSTOL", "0.0"),
					      ("RELTOL", "0.0")]
  | solver2params (RK4 {dt}) = [("DT", r2s dt),
				("ABSTOL", "0.0"),
				("RELTOL", "0.0")]
  | solver2params (MIDPOINT {dt}) = [("DT", r2s dt),
				     ("ABSTOL", "0.0"),
				     ("RELTOL", "0.0")]
  | solver2params (HEUN {dt}) = [("DT", r2s dt),
				 ("ABSTOL", "0.0"),
				 ("RELTOL", "0.0")]
  | solver2params (ODE23 {dt, abs_tolerance, rel_tolerance}) = 
    [("DT", r2s dt),
     ("ABSTOL", r2s abs_tolerance),
     ("RELTOL", r2s rel_tolerance)]
  | solver2params (ODE45 {dt, abs_tolerance, rel_tolerance}) = 
    [("DT", r2s dt),
     ("ABSTOL", r2s abs_tolerance),
     ("RELTOL", r2s rel_tolerance)]
  | solver2params (CVODE {dt, abs_tolerance, rel_tolerance, lmm, iter, solv}) = 
    [("DT", r2s dt),
     ("ABSTOL", r2s abs_tolerance),
     ("RELTOL", r2s rel_tolerance),
     ("CVODE_LMM", case lmm of CV_ADAMS => "CV_ADAMS" | CV_BDF => "CV_BDF"),
     ("CVODE_ITER", case iter of CV_FUNCTIONAL => "CV_FUNCTIONAL" | CV_NEWTON => "CV_NEWTON")] @ 
    cvode_solver2params solv

end
