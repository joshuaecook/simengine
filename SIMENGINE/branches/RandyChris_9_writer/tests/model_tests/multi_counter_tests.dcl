# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Thu Oct 23 11:13:47 -0400 2008
# ========================================================================

$s = compile(tests/models/multi-counter.dsl, ver)
success($s)
$last.close_if_fail()

$s.enable_output(x*)
$s.set_param(gain1, 1)
$s.set_param(gain2, 5)
$s.set_param(gain3, 10)
$s.set_param(gain4, 50)

$s.runfor(10)
$s.finish()

$s.traces

assert(all(diff($s.traces.t)==1))

$first_x1 = calc(first($s.traces.x1))
$last_x1 = calc(last($s.traces.x1))
$first_x2 = calc(first($s.traces.x2))
$last_x2 = calc(last($s.traces.x2))
$first_x3 = calc(first($s.traces.x3))
$last_x3 = calc(last($s.traces.x3))
$first_x4 = calc(first($s.traces.x4))
$last_x4 = calc(last($s.traces.x4))

assert($first_x1 == 0)
assert($last_x1 == 1*10)
assert($first_x2 == 0)
assert($last_x2 == 5*10)
assert($first_x3 == 0)
assert($last_x3 == 10*10)
assert($first_x4 == 0)
assert($last_x4 == 50*10)

$s.output

assertion_report