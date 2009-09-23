//We should revisit this counter example, we really need the syntax to do difference equations in order to do this properly

model Multi_Counter

visible state x1 (-1000 to 100000 by 0.01) = 0
visible state x2 (-1000 to 100000 by 0.01) = 0
visible state x3 (-1000 to 100000 by 0.01) = 0
visible state x4 (-1000 to 100000 by 0.01) = 0
state y (-10 to 10 by 0.0001) = 0
parameter gain1 (-10 to 1000 by 0.01) = -1
parameter gain2 (-10 to 1000 by 0.01) = -1
parameter gain3 (-10 to 1000 by 0.01) = -1
parameter gain4 (-10 to 1000 by 0.01) = -1

equations
  x1' = gain1
  x2' = gain2
  x3' = gain3
  x4' = gain4
  y_[1] = y*y*y*y
end

t.setPrecision (Range.new(0, 1000, 1))
solver = forwardeuler(1)

setVisible |t|
end
