model (x) = OpTest2()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 0..3 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output x = (x) with {structure}
   output t_x = (transpose(x)) with {structure}
   output t_t_x = (transpose(transpose(x))) with {structure}
end
