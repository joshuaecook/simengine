model Sines

  model SineWave
    visible state x (-300 to 300 by 0.00001) = 0
    visible state y (-300 to 300 by 0.00001) = 0

    equations
      x' = 1 - y
      y' = x - 1
    end

  end

  submodel waves = [SineWave.new() foreach x in 1..2]

  solver = midpoint(0.1)
  solver.maxduration = 1000
end