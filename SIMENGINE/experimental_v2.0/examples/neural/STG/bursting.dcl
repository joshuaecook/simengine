#compile model and set parameters for bursting behavior
$s = compile(stg.dsl)
$s.set_param(gNa, 100)
$s.set_param(gCaT, 0)
$s.set_param(gCaS, 4)
$s.set_param(gA, 0) 
$s.set_param(gKCa, 15)
$s.set_param(gKd, 50)
$s.set_param(gh, 0.02)
$s.set_param(gleak, 0.03)

$s.enable_output(Vm)

$s.runfor(5000)

