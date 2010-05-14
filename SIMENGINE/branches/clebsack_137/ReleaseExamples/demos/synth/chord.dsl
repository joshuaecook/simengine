// Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.
// Chord synthesis from multi-tone harmonics

// tonic = frequency of base note of chord
// third, fifth, seventh = half steps above the note for the base frequency for additional chord notes

import "harmonics.dsl"

model (y) = chord (tonic, third, fifth)

input tonic with {default = 110} // 110Hz = A2
input third with {default = 4} // major third
input fifth with {default = 7} // major fifth

// Notes are generated in 12-TET, see midiscale.dsl for more information

submodel harmonics root with {fundamental = tonic}
submodel harmonics three with {fundamental = (2^(1/12))^third * tonic}
submodel harmonics five with {fundamental = (2^(1/12))^fifth * tonic}
submodel harmonics octave with {fundamental = 2 * tonic}

output y = 1/4 * (root.y + three.y + five.y + octave.y)

solver = forwardeuler
solver.dt = 1/48e3

end
