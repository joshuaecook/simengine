//Copyright (C) 2010 by Simatra Modeling Technologies, L.L.C.

import "harmonics.dsl"

model (Vm) = chord (tonic)

input tonic with {default = 55} // 55Hz = A1

submodel harmonics root with {fundamental = tonic}
submodel harmonics third with {fundamental = 5/4 * tonic}
submodel harmonics fifth with {fundamental = 3/2 * tonic}
submodel harmonics octave with {fundamental = 2 * tonic}

output Vm = root.Vm + third.Vm + fifth.Vm + octave.Vm

solver = rk4{dt=1/4096} // 4kHz sampling frequency

end
