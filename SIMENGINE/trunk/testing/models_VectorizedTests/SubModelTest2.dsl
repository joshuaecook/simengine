model (x) = vector_x()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 0 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output x = (x)
end


model (x) = SubModelTest2()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   submodel vector_x s @(tensor [5])
   
   // Output definitions
   output x = (s.x)

end
