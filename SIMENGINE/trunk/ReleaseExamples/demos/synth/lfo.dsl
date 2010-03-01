//Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

model (y) = lfo (w, phi)

state y = 0

equation r = 2 * t * pi
equation y[t] = sin(w * r + phi)

solver = rk4{dt=1/4096}

end
