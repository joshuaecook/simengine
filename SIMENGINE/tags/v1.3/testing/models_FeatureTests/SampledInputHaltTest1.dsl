model (o) = SampledInputHaltTest1(i)
  iterator n with {discrete, sample_period=1}
  input i with {iter=n, halt_when_exhausted}
  output o = i
end
