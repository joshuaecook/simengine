model (w) = w_equ(u, b0, b1, e, I)
  state w = 0
  equation w' = e * (b0 + b1 * u - w)
end

