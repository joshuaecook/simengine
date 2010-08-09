model (o) = SampledInputHaltTest4(i)
  iterator n with {discrete, sample_period=1}
  iterator m with {discrete, sample_period=2}
  input i with {default = 0, iter=m, halt_when_exhausted}
  output o[n] = i
end
