model (y_eq, y_ne, y_gt, y_lt, y_ge, y_le) = FunctionTestRelational()

  t {solver=forwardeuler{dt=1}}

  state x = 0
  equation x' = 1

  constant threshold = 5

  output y_eq = x with {condition= x == 5}
  output y_ne = x with {condition= x <> 5}
  output y_gt = x with {condition= x > 5}
  output y_lt = x with {condition= x < 5}
  output y_ge = x with {condition= x >= 5}
  output y_le = x with {condition= x <= 5}

end
