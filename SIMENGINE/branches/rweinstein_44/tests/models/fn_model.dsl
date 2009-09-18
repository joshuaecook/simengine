// model definition
model (u,w) = fn (b0, b1, e, I)
  input b0 with {default=2}
  input b1 with {default=1.5}
  input e with {default=0.1}
  input I with {default=2}
//   parameter b0 (1 to 4 by 0.01) = 2
//   parameter b1 (1 to 4 by 0.01) = 1.5
//   parameter e (0.01 to 0.3 by 0.01) = 0.1
//   parameter I (0 to 5 by 0.01) = 2
  
  state u (-4 to 4 by 0.001) = 1
  state w (-4 to 4 by 0.001) = 1
  
  //function cube (x) = x * x * x


  equations
    //d(u) = u - cube(u) / 3 - w + I
    u' = u - u * u * u / 3 - w + I
    w' = e * (b0 + b1 * u - w)
  end

//  solver = rk4(0.1)
  solver = forwardeuler(0.1)

//  solver.maxduration = 1000000

  //t.setPrecision (Range.new(0, 1000, 0.01))

  //setVisible |u|
  //setVisible |w|
end
//compile fn
