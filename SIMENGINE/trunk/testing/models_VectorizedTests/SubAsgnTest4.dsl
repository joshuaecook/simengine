model (x) = SubAsgnTest4()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = 1..4 with {iter=ModelTime}

   // Differential equation definitions
   equations
      c = [4,3,2,1]
      x[i=_]' = c[3-i]
   end

   // Output definitions
   output x = (x)
end
