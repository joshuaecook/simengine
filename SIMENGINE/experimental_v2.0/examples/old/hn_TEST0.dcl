$s = compile(hn.dsl)
$s.enable_output(Vm)
$s.runfor(20)

calc([a, b] = max($s.traces.Vm))
$VmMaxS = calc(a)
$tMaxS = calc($s.traces.t(b))
calc([a,b] = min($s.traces.Vm))
$VmMinS = calc(a)
$tMinS = calc($s.traces.t(b))

$s.reset()
$s.set_param(Eleak, -62.5)
$s.set_param(gleak, 11)
$s.runfor(8)

calc([a, b] = max($s.traces.Vm))
$VmMaxB = calc(a)
$tMaxB = calc($s.traces.t(b))
calc([a,b] = min($s.traces.Vm))
$VmMinB = calc(a)
$tMinB = calc($s.traces.t(b))

assert($VmMaxS == 2.9128)
assert($VmMinS == -45.975)
assert($tMaxS == 0.1792)
assert($tMinS == 19.979)

assert($VmMaxB == 2.4233)
assert($VmMinB == -54.674)
assert($tMaxB == 4.311)
assert($tMinB == 0.6451)

assertion_report
