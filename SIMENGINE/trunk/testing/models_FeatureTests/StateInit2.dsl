model (a) = subby (init)
  state a = init
  equation a' = 1
end

model (x,y) = StateInit2
  submodel subby s1 with {init=2}
  submodel subby s2 with {init=3}

  equation x = s1.a
  equation y = s2.a
  solver = forwardeuler {dt = 1}
end
