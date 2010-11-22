model (x) = SubAsgnTest3()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      c = [1,2,3,4]
      x[i=_]' = c[i]
   end

   // Output definitions
   output x = (x)
end
