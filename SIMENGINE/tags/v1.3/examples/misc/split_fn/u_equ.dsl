model (u) = u_equ(w, b0, b1, e, I)
  state u = 0
  equation u' = u - u*u*u / 3 - w + I
  equation u = 5 when u > 5 and I < 2
  equation u = 10 when u <10 and I > 5
end

