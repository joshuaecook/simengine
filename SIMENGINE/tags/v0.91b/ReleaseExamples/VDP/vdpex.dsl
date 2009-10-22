/* 
 *   Adapted from the Van der Pol model
 *   Copyright 2009, Simatra Modeling Technologies, L.L.C.
 */

model (y) = vdpex

  state y = 2
  state y1 = 0

  equations
    y' = y1
    y1' = 1000*(1-y^2)*y1-y
  end

end
