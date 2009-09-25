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

(*
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
*)
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

end
