model (x) = StateRangeTest1()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 0..100 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output x = (x) with {structure}
end
