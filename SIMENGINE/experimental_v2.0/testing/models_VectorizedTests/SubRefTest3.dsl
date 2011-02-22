model (x) = SubRefTest3()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output explicit = (x[0:3])
   output implicit = (x[_]) // _ is the wildcard
   output none = (x[()]) // maps to unit
end
