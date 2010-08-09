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
$s.runfor(1100)
$s.enable_output(Vm)
$s.runfor(700)

$spikeCountB = calc(length(find($s.traces.Vm(2:end) >=0 & $s.traces.Vm(1:end-1) < 0)))
calc([a,b] = max($s.traces.Vm))
$VmMaxB = calc(a)
$tMaxB = calc($s.traces.t(b))

#reset and set spiking paramters
$s.reset()
$s.disable_output(*)
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

$spikeCountS = calc(length(find($s.traces.Vm(2:end) >=0 & $s.traces.Vm(1:end-1) < 0)))
calc([a,b] = max($s.traces.Vm))
$VmMaxS = calc(a)
$tMaxS = calc($s.traces.t(b))

assert($spikeCountB == 15)
assert($VmMaxB == 47.223)
assert($tMaxB == 389.09)
assert($spikeCountS == 54)
assert($VmMaxS == 40.398)
assert($tMaxS == 27.62)

assertion_report