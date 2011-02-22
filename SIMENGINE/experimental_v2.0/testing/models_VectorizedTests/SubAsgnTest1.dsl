model (x) = SubAsgnTest1()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      x[0]' = 1
      x[1]' = 2
      x[2]' = 3
      x[3]' = 4
   end

   // Output definitions
   output x = (x)
end
