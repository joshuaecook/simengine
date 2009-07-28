// model definition
settings.debug.logdof.setValue(true)
import "u_equ.dsl"
import "w_equ.dsl"

model (top_u, top_w) = split_fn(I)

  input I with {default=2}

  constant b0 = 2
  constant b1 = 1.5
  constant e = 0.1
  
  submodel u_equ u_eq with {b0=b0, b1=b1, e=e, I=I}
  equation top_u = u_eq.u
  submodel w_equ w_eq with {u=top_u, b0=b0, b1=b1, e=e, I=I}
  equation top_w = w_eq.w

  u_eq.w = w_eq.w

//  solver = rk4(0.1)

    solver = ode45// with {dt=0.1, abstol=1e-6, reltol=1e-3, max_t=100}
    solver.dt = 0.1
    solver.max_t = 100

end


