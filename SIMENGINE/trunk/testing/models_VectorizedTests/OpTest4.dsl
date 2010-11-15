model (x) = OpTest4()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state M = [[1, 2], [3, 4]] with {iter=ModelTime}
   state b = [1, 2] with {iter=ModelTime}

   // Differential equation definitions
   equations
      b' = 1
   end

   // Output definitions
   output x = (linear_solve(M,b))
end
