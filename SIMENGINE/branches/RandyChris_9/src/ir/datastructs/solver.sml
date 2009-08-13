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
  | solver2name (ODE23 _) = "bogacki_shampine"
  | solver2name (ODE45 _) = "dormand_prince"
  | solver2name (CVODE _) = "cvode"

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
