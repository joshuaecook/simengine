// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
// Harmonic synthesis from fundamental frequency

// fundamental = frequency of fundamental tone
// y = output waveform of note with harmonics superimposed

import "lfo.dsl"

model (y) = harmonics (fundamental)

input fundamental with {default = 55}

// Fundamental tone
submodel lfo first with {w = fundamental}

// Upper harmonics
submodel lfo second with {w = fundamental * 2}
submodel lfo third with {w = fundamental * 3}
submodel lfo fourth with {w = fundamental * 4}
submodel lfo fifth with {w = fundamental * 5}
submodel lfo sixth with {w = fundamental * 6}
submodel lfo seventh with {w = fundamental * 7}
submodel lfo eighth with {w = fundamental * 8}
submodel lfo ninth with {w = fundamental * 9}
submodel lfo tenth with {w = fundamental * 10}

// Lower harmonics
submodel lfo onehalf with {w = fundamental / 2}
submodel lfo onethird with {w = fundamental / 3}
submodel lfo onefourth with {w = fundamental / 4}
submodel lfo onefifth with {w = fundamental / 5}
submodel lfo onesixth with {w = fundamental / 6}
submodel lfo oneseventh with {w = fundamental / 7}
submodel lfo oneeighth with {w = fundamental / 8}
submodel lfo onenineth with {w = fundamental / 9}
submodel lfo onetenth with {w = fundamental / 10}

// Harmonic superposition
output y = (1/2 * first.y +
	     1/8 * onehalf.y +
	     1/8 * second.y +
             1/16 * onethird.y +
	     1/16 * third.y +
             1/32 * onefourth.y +
	     1/32 * fourth.y +
	     1/64 * onefifth.y +
	     1/64 * fifth.y +
	     1/128 * onesixth.y +
	     1/128 * sixth.y +
	     1/256 * oneseventh.y +
	     1/256 * seventh.y +
	     1/512 * oneeighth.y +
	     1/512 * eighth.y +
	     1/1024 * onenineth.y +
	     1/1024 * ninth.y +
	     1/2048 * onetenth.y +
	     1/2048 * tenth.y)

solver = forwardeuler
solver.dt = 1/48e3

end
