/*
 * spikes - Create spike events from input voltage trace data
 *
 * Copyright 2010 Simatra Modeling Technologies
 */
model (events) = spikes(Vm)

  // set up a discrete iterator with steps of 0.01 milliseconds
  iterator i with {discrete, sample_period=1}

  // Define a sampled input for the voltage trace
  input Vm with {iter=i, halt_when_exhausted}

  // Determine when a spike occurs
  equation spike_occurred = Vm > 20 and Vm < Vm[i-1] and Vm[i-1] >= Vm[i-2]

  // Create an event output
  output events = () when spike_occurred
end
