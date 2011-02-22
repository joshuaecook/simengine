// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
// Low Frequency Oscillator

// w = frequency
// phi = phase angle
// y = output waveform, which is set to 0 if frequency is above 20kHz

model (y) = lfo (w, phi)

input phi with {default = 0}

equations
  r = 2 * t * pi
  y = {sin(w * r + phi) when w <= 2e4 or w > 0, 0 otherwise}
end

solver = forwardeuler
solver.dt = 1/48e3

end
