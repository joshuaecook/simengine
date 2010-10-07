$s = compile(pbc.dsl)
$s.set_param(Iext, 15)
$s.runfor(3)
$s.enable_output(Vm)
$s.runfor(10)

$spikeCount = calc(length(find($s.traces.Vm(2:end) >= 0 & $s.traces.Vm(1:end-1) < 0)))
calc([a,b] = max($s.traces.Vm))
$VmMax = calc(a)
$tMax = calc($s.traces.t(b))

assert($spikeCount == 21)
assert($VmMax == 9.8738)
assert($tMax == 4.506)

assertion_report