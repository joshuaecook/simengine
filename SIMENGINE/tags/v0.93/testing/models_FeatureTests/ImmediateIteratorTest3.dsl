model (F) = Force (m, a)
  equation F = m * a
end

model (F) = ImmediateIteratorTest3 (r)
   input r with {default = 1}

   constant G = 9.80665
   constant mass1 = 10

   equation mass2 = mass1 * 3

   submodel Force force with {m = (mass1 * mass2) / (r * r), a = G}

   output F = force.F
end
