model (y) = PostProcessIntermediateIteratorTest2()

  state p = 0
  equation p = p + 1

  output y = (p, p[t[-2]])

  t{solver=forwardeuler{dt=1}}

end
