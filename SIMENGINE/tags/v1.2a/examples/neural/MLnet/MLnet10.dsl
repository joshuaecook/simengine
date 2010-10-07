import "ML.dsl"

model (spikes) = MLnet10()
  submodel ML cell_1 with {I = 10}
  submodel ML cell_2 
  submodel ML cell_3 
  submodel ML cell_4 
  submodel ML cell_5 
  submodel ML cell_6 
  submodel ML cell_7 
  submodel ML cell_8 
  submodel ML cell_9 
  submodel ML cell_10 

  cell_1.spikesIn = cell_9.spikesOut + cell_10.spikesOut + cell_2.spikesOut + cell_3.spikesOut
  cell_2.spikesIn = cell_10.spikesOut + cell_1.spikesOut + cell_3.spikesOut + cell_4.spikesOut
  cell_3.spikesIn = cell_1.spikesOut + cell_2.spikesOut + cell_4.spikesOut + cell_5.spikesOut
  cell_4.spikesIn = cell_2.spikesOut + cell_3.spikesOut + cell_5.spikesOut + cell_6.spikesOut
  cell_5.spikesIn = cell_3.spikesOut + cell_4.spikesOut + cell_6.spikesOut + cell_7.spikesOut
  cell_6.spikesIn = cell_4.spikesOut + cell_5.spikesOut + cell_7.spikesOut + cell_8.spikesOut
  cell_7.spikesIn = cell_5.spikesOut + cell_6.spikesOut + cell_8.spikesOut + cell_9.spikesOut
  cell_8.spikesIn = cell_6.spikesOut + cell_7.spikesOut + cell_9.spikesOut + cell_10.spikesOut
  cell_9.spikesIn = cell_7.spikesOut + cell_8.spikesOut + cell_10.spikesOut + cell_1.spikesOut
  cell_10.spikesIn = cell_8.spikesOut + cell_9.spikesOut + cell_1.spikesOut + cell_2.spikesOut

  
  output spikes = (cell_1.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut,
                   cell_2.spikesOut) with {condition = cell_1.spikesOut > 0 or cell_2.spikesOut > 0}

solver = forwardeuler
solver.dt = .02
end

