model (y,ay,yh,ayh) = FunctionTestTrig(low,high)

  input low with {default = 0}
  input high with {default = pi}

  state x = 0
  equations
    x' = 0.01
    sx = x*(high-low) + low
    y1 = sin(sx)
    y2 = cos(sx)
    y3 = tan(sx)
    y4 = csc(sx)
    y5 = sec(sx)
    y6 = cot(sx)

    ay1 = asin(sx)
    ay2 = acos(sx)
    ay3 = atan(sx)
    ay4 = acsc(sx)
    ay5 = asec(sx)
    ay6 = acot(sx)

    yh1 = sinh(sx)
    yh2 = cosh(sx)
    yh3 = tanh(sx)
    yh4 = csch(sx)
    yh5 = sech(sx)
    yh6 = coth(sx)

    ayh1 = asinh(sx)
    ayh2 = acosh(sx)
    ayh3 = atanh(sx)
    ayh4 = acsch(sx)
    ayh5 = asech(sx)
    ayh6 = acoth(sx)
  end

  output y = (sx, y1, y2, y3, y4, y5, y6)
  output ay = (sx, ay1, ay2, ay3, ay4, ay5, ay6)
  output yh = (sx, yh1, yh2, yh3, yh4, yh5, yh6)
  output ayh = (sx, ayh1, ayh2, ayh3, ayh4, ayh5, ayh6)

  solver = forwardeuler{dt=1}

end