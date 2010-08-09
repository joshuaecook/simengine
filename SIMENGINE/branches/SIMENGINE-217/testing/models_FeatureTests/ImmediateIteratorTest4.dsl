model (F) = Force (m1, m2, r, a)
  equation m = (m1 * m2) / (r * r)
  equation F = m * a
end

model (F) = ImmediateIteratorTest4 (r)
   input r with {default = 1}

   constant G = 9.80665

   submodel Force force with {m1 = 10, m2 = 30, r = r, a = G}

   output F = force.F
end
