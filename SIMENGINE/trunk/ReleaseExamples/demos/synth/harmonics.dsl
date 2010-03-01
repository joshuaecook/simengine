//Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

import "lfo.dsl"

model (Vm) = harmonics (fundamental)

input fundamental with {default = 55}

submodel lfo first with {w = fundamental, phi = 0}
submodel lfo second with {w = 2 * fundamental, phi = 0}
submodel lfo third with {w = 3 * fundamental, phi = 0}
submodel lfo fourth with {w = 4 * fundamental, phi = 0}
submodel lfo fifth with {w = 5 * fundamental, phi = 0}
submodel lfo sixth with {w = 6 * fundamental, phi = 0}
submodel lfo seventh with {w = 7 * fundamental, phi = 0}

output Vm = first.y + (second.y/2) + (third.y/4) + (fourth.y/8) + (fifth.y/16) + (sixth.y/32) + (seventh.y/64)

solver = rk4{dt=1/4096}

end
