model neuron
  state x rangeinfoopt = exp
  parameter p rangeinfoopt = exp

  d(x) = exp
  y = exp // intermediate?
  
  input z

  external input a rangeinfo

  function f (x) = x+1

  task t(x)
    x+1
  end

  model axon
  end

  model n = axon.new()

end

model q = neuron.new() // IS THIS HOW TO INSTANTIATE?
q.z = equation d(z) = x + 1 // HOW DOES SCOPING HERE WORK?  CAN IT SEE LOCAL VARS AS WELL?
