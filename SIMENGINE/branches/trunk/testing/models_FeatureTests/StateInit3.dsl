model (a) = subby (init)
  state a = init
  equation a' = -init
end

model (x,y) = StateInit3 (init)
  submodel subby s1 with {init=2}
  submodel subby s2 with {init=init}

  equation x = s1.a
  equation y = s2.a
  solver = forwardeuler {dt = 1}
end
