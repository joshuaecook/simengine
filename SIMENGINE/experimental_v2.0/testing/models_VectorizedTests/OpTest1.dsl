model (x) = OpTest1()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = [[0, 0], [0, 0], [0, 0]] with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output x = (x) with {structure}
   output transpose_x = (transpose(x)) with {structure}
end
