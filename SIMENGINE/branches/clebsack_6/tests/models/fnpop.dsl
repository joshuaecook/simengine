import "fn_model.dsl"

model (u,w) = fnpop (ave_I)
  input ave_I with {default=2}

  submodel fn fn1 with {b0=2, b1=1.5, e=0.1, I=ave_I-0.75}
  submodel fn fn2 with {b0=2, b1=1.5, e=0.1, I=ave_I}
  submodel fn fn3 with {b0=2, b1=1.5, e=0.1, I=ave_I+0.75}

  output u = (fn1.u, fn2.u, fn3.u)
  output w = (fn1.w, fn2.w, fn3.w)
end

/*
model (u, w) = fnpop //(ave_I)
  //input ave_I with {default=2}

  submodel fn fn1 with {b0=2, b1=1.5, e=0.1, I=1}
  submodel fn fn2 with {b0=2, b1=1.5, e=0.1, I=2}
  submodel fn fn3 with {b0=2, b1=1.5, e=0.1, I=3}

  output u = (fn1.u, fn2.u, fn3.u)
  output w = (fn1.w, fn2.w, fn3.w)
end
*/
compile(fnpop)
