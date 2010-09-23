#compile model and set parameters for bursting behavior
$s = compile(stg.dsl)
$s.set_param(gNa, 500)
$s.set_param(gCaT, 0)
$s.set_param(gCaS, 0)
$s.set_param(gA, 40) 
$s.set_param(gKCa, 0)
$s.set_param(gKd, 75)
$s.set_param(gh, 0.01)
$s.set_param(gleak, 0)

$s.enable_output(Vm)

$s.runfor(100)
plot($s)
