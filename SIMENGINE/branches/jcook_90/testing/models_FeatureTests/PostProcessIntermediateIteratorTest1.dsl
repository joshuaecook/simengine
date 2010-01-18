settings.debug.logdof.setValue(true)
model (y) = PostProcessIntermediateIteratorTest1()

  equation x = t

  output y = (x, x[t[-2]])

  t{solver=forwardeuler{dt=1}}

end
