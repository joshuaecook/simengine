model (x) = SubRefTest4()

   // Iterator definitions
   iterator ModelTime with {continuous, solver=forwardeuler{dt=1}}

   // State definitions
   state x = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] with {iter=ModelTime}

   // Differential equation definitions
   equations
      x' = 1
   end

   // Output definitions
   output none = (x[(),()])
   output all = (x[_,_]) with {notime, structure}
   output row1 = (x[0,_]) with {notime, structure}
   output row2 = (x[1,_]) with {notime, structure}
   output row3 = (x[2,_]) with {notime, structure}
   output col1 = (x[_,0]) with {notime, structure}
   output col2 = (x[_,1]) with {notime, structure}
   output col3 = (x[_,2]) with {notime, structure}
   output diag1 = (concat([x[0,0], x[1,1], x[2,2]])) with {notime, structure}
   output diag2 = ([x[0,2], x[1,1], x[2,0]]) with {notime, structure}
   output corners = (x[0:2:2,0:2:2]) with {notime, structure}
end
