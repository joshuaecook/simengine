#compile model and set parameters for bursting behavior
$s = compile(stg.dsl)
$s.set_param(gNa, 100)
$s.set_param(gCaT, 0)
$s.set_param(gCaS, 10)
$s.set_param(gA, 40) 
$s.set_param(gKCa, 5)
$s.set_param(gKd, 75)
$s.set_param(gh, 0.02)
$s.set_param(gleak, 0.03)
#let state settle
$s.runfor(1000)

$s.enable_output(Vm)
$s.runfor(1000)
plot($s)
