model SineWave
  visible state x (-300 to 300 by 0.00001) = 0
  visible state y (-300 to 300 by 0.00001) = 0

  equations
    x' = 1 - y
    y' = x - 1
  end

  t.setPrecision (Range.new(0, 1000, 0.01))
  
  solver = midpoint(0.1)
end
