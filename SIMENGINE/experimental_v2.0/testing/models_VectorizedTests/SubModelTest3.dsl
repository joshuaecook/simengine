model (x) = vector_x()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // Input definitions
   input I @(tensor [5])

   // State definitions
   state x = I with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output x = (x)
end

model (x) = SubModelTest3()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   submodel vector_x s
   s.I = 0..4
   
   // Output definitions
   output x = (s.x) with {structure}

end
