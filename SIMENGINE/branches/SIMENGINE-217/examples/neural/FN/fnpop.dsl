import "fn.dsl"

model (u,w) = fnpop
  iterator i = 1..1000
  submodel fn neurons [i]
  output u = neurons.u
  output w = neurons.w
end
