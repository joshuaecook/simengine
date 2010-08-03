$s = compile(hn_hco.dsl)
$s.enable_output(*Vm)

$s.runfor(20)
$VmMaxL3 = calc(max($s.traces.HNL3.Vm))
$VmMaxR3 = calc(max($s.traces.HNR3.Vm))
$VmMinL3 = calc(min($s.traces.HNL3.Vm))
$VmMinR3 = calc(min($s.traces.HNR3.Vm))

assert($VmMaxL3 == 3.2955)
assert($VmMinL3 == -59.998)
assert($VmMaxR3 == 3.5631)
assert($VmMinR3 == -58.437)

assertion_report