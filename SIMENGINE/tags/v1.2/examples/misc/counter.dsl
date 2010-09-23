//We should revisit this counter example, we really need the syntax to do difference equations in order to do this properly

model (x)=counter(gain)

state x = 0
input gain with {default=-1}

equations
  x' = gain
end

solver = forwardeuler
solver.dt = 1
solver.max_t = 1000

end
