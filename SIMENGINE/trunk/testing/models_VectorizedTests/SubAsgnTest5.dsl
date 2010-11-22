model (x) = SubAsgnTest5()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      c = [3, 1, 4, 2]
      indices = [1, 3, 0, 2]
      x[i=_]' = c[indices[i]]
   end

   // Output definitions
   output x = (x)
end
