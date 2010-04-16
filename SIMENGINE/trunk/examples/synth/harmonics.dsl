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

output Vm = (1/2 * first.y +
	     1/4 * second.y +
	     1/8 * third.y +
	     1/16 * fourth.y +
	     1/32 * fifth.y +
	     1/64 * sixth.y +
	     1/128 * seventh.y)

end
