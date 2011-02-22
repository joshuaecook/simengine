model (o) = SampledInputCycleTest1(i)
  iterator n with {discrete, sample_period=1}
  input i with {default = 0, iter=n, cycle_when_exhausted}
  output o = i
end
