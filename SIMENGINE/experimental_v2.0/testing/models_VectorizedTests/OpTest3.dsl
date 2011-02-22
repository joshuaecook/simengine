model (x) = OpTest3()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = [[1, 2], [3, 4], [5, 6]] with {iter=ModelTime}
   state y = -[[1, 2, 3], [4, 5, 6]] with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
      y' = -1
   end

   // Output definitions
   output mul1 = (matrix_mul (x,y)) with {structure}
   output mul2 = (matrix_mul (y,x)) with {structure}
end
