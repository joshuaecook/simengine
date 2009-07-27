model Simple
  state x (0 to 100 by 0.1) = 0

  equations
    x' = {sin(t) when not(t < 10), cos(t) otherwise}
  end

  t.setPrecision (Range.new(0, 1000, 0.01))
  settings.integrationMethod.dt = dt
  dt.setInitialValue(0.1)
  dt.setPrecision (Range.new(0, 1, 0.01))

  setVisible |x|
  setVisible |t|
end
