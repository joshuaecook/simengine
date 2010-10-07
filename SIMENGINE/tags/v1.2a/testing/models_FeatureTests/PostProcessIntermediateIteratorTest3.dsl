model (y) = PostProcessIntermediateIteratorTest3()

  state p = 0
  equation p' = 0
  equation p = p + 1 when true // Create an update iterator

  output y = (p, p[t[-2]])

  t{solver=forwardeuler{dt=1}}

end
