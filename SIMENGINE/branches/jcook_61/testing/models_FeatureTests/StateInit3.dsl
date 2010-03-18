model (a) = subby (i)
  state a = i
  equation a' = -a
end

model (x,y) = StateInit3 (z)
  submodel subby s1 with {i=2}
  submodel subby s2 with {i=z}

  equation x = s1.a
  equation y = s2.a
end
