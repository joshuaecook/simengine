$s = compile(brk.dsl)
$s.enable_output(*)
$s.set_param(Iext, 10)
$s.runfor(10)

$minVs = calc(min($s.traces.Vs))
$maxVs = calc(max($s.traces.Vs))
$areaVs = calc(sum($s.traces.Vs - $minVs))
$minVd = calc(min($s.traces.Vd))
$maxVd = calc(max($s.traces.Vd))
$areaVd = calc(sum($s.traces.Vd - $minVd))

assert($minVs == -60)
assert($maxVs == 51.473)
assert($areaVs == 53115)
assert($minVd == -60)
assert($maxVd == -55.119)
assert($areaVd == 10983)

assertion_report