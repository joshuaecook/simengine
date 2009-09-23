structure Solver =
struct

datatype solver =
	 FORWARD_EULER of {dt:real}
       | EXPONENTIAL_EULER of {dt:real}
       | RK4 of {dt:real}
       | MIDPOINT of {dt:real}
       | HEUN of {dt:real}
       | ODE23 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | ODE45 of {dt:real, abs_tolerance: real, rel_tolerance: real}
       | CVODE of {dt:real, abs_tolerance: real, rel_tolerance: real}

val r2s = Util.r2s

val default = ODE45 {dt=0.1, abs_tolerance=(1e~6), rel_tolerance=(1e~3)}

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
  | solver2params (CVODE {dt, abs_tolerance, rel_tolerance}) = 
    [("DT", r2s dt),
     ("ABSTOL", r2s abs_tolerance),
     ("RELTOL", r2s rel_tolerance)]

end
