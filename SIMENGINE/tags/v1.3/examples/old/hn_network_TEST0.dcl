$s = compile(hn_network.dsl)
$s.enable_output(*Vm)
$s.runfor(20)

calc([a,b] = max($s.traces.HNL3.Vm));
$VmMaxL3 = calc(a);
$tMaxL3 = calc($s.traces.t(b));

calc([a,b] = max($s.traces.HNR3.Vm));
$VmMaxR3 = calc(a);
$tMaxR3 = calc($s.traces.t(b));

calc([a,b] = max($s.traces.HNL4.Vm));
$VmMaxL4 = calc(a);
$tMaxL4 = calc($s.traces.t(b));

calc([a,b] = max($s.traces.HNR4.Vm));
$VmMaxR4 = calc(a);
$tMaxR4 = calc($s.traces.t(b));

calc([a,b] = max($s.traces.HNR12.Vm));
$VmMaxR12 = calc(a);
$tMaxR12 = calc($s.traces.t(b));

calc([a,b] = max($s.traces.HNL12.Vm));
$VmMaxL12 = calc(a);
$tMaxL12 = calc($s.traces.t(b));

assert($VmMaxL3 == 3.4979)
assert($VmMaxR3 == 3.1336)
assert($VmMaxL4 == 3.3398)
assert($VmMaxR4 == 3.4339)
assert($VmMaxL12 == 5.3354)
assert($VmMaxR12 == 5.3359)

assert($tMaxL3 == 1.944)
assert($tMaxR3 == 11.546)
assert($tMaxL4 == 2.0125)
assert($tMaxR4 == 1.9463)
assert($tMaxL12 == 13.528)
assert($tMaxR12 == 6.6835)

assertion_report