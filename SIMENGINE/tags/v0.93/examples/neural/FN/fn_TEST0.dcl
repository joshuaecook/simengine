$s = compile(fn.dsl)
$s.enable_output(*)

$s.runfor(50)

$uMin = calc(min($s.traces.u))
$uMax = calc(max($s.traces.u))
$uSum = calc(sum($s.traces.u - $uMin))

$wMin = calc(min($s.traces.w))
$wMax = calc(max($s.traces.w))
$wSum = calc(sum($s.traces.w - $wMin))

assert($uMin == -1.881)
assert($uMax == 2.2057)
assert($uSum == 1036.9)
assert($wMin == 0)
assert($wMax == 2.9236)
assert($wSum == 950.48)

assertion_report