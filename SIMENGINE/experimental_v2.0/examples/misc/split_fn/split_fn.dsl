// model definition
settings.debug.generateC.setValue(true)
settings.debug.logdof.setValue(true)
import "u_equ.dsl"
import "w_equ.dsl"

model (u, w, I) = split_fn(b0, b1, e)
  input b0 with {default=2}
  input b1 with {default=1.5}
  input e with {default=0.1}

  state I = 0 with {iter=n}

  equation I[n+1] = I[n] + 0.5*n
  
  submodel u_equ u_eq with {b0=b0, b1=b1, e=e, I=I}
  equation u = u_eq.u
  submodel w_equ w_eq with {u=u, b0=b0, b1=b1, e=e, I=I}
  equation w = w_eq.w

  u_eq.w = w

  solver = ode45
end


