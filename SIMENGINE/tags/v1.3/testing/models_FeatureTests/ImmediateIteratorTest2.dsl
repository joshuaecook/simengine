// An intermediate equation should be associated with the "always" iterator
// iff all of its terms are constants, top-level inputs, 
// or other "always" intermediates
model (F) = ImmediateIteratorTest2 (r)
   input r with {default = 1}

   constant G = 9.80665
   constant mass1 = 10

   equation mass2 = mass1 * 3

   equation F = G * (mass1 * mass2) / (r * r)
end
