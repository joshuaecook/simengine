model (x) = SubAsgnTest2()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x[i=_]' = i+1
   end

   // Output definitions
   output x = (x)
end
