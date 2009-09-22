//We should revisit this counter example, we really need the syntax to do difference equations in order to do this properly

model Counter

visible state x (-1000 to 1000 by 0.01) = 0
parameter gain (-10 to 10 by 0.01) = -1

equations
  x' = gain
end

solver = forwardeuler(1)
solver.maxduration = 1000

setVisible |t|
end
