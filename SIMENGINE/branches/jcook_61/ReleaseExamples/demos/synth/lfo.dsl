//Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

model (y) = lfo (w, phi)

equation r = 2 * t * pi
output y = sin(w * r + phi)

end
