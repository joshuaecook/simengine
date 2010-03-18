model (a) = subby (i)
  state a = i
  equation a' = -a
end

model (x,y) = StateInit2
  submodel subby s1 with {i=2}
  submodel subby s2 with {i=3}

  equation x = s1.a
  equation y = s2.a
end
