#compile model and set parameters for aperiodic behavior
$s = compile(stg.dsl)
$s.set_param(gNa, 300)
$s.set_param(gCaT, 0)
$s.set_param(gCaS, 10)
$s.set_param(gA, 20) 
$s.set_param(gKCa, 20)
$s.set_param(gKd, 125)
$s.set_param(gh, 0.05)
$s.set_param(gleak, 0.01)
$s.runfor(1000)
$s.enable_output(Vm)
$s.runfor(1000)
plot($s)