//Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

import "harmonics.dsl"

model (Vm) = chord (tonic, third, fifth)

input tonic with {default = 110} // 110Hz = A2
input third with {default = 4/5} // major third
input fifth with {default = 3/2} // major fifth

submodel harmonics root with {fundamental = tonic}
submodel harmonics three with {fundamental = third * tonic}
submodel harmonics five with {fundamental = fifth * tonic}
submodel harmonics octave with {fundamental = 2 * tonic}

output Vm = 1/4 * (root.Vm + three.Vm + five.Vm + octave.Vm)

end
