$s = compile(hh.dsl)
$s.enable_output(Vm)
$s.runfor(40)

calc(zeroCross = find($s.traces.Vm(2:end) > 0 & $s.traces.Vm(1:end-1) <= 0))
$cross1 = calc($s.traces.t(zeroCross(1)))
$cross2 = calc($s.traces.t(zeroCross(2)))
$cross3 = calc($s.traces.t(zeroCross(3)))

assert($cross1 == 9.547)
assert($cross2 == 21.534)
assert($cross3 == 33.521)

$VmMin = calc(min($s.traces.Vm))
$VmMax = calc(max($s.traces.Vm))
$VmSum = calc(sum($s.traces.Vm - $VmMin))

assert($VmMin == -73.525)
assert($VmMax == 22.609)
assert($VmSum == 731480.0)

assertion_report