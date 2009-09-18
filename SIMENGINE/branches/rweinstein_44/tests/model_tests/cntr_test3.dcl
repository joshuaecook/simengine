# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Wed Oct 22 11:59:40 -0400 2008
# ========================================================================

$param = 2

# using ver
$s_ver = compile(../models/counter.dsl, ver)
success($s_ver)
$last.close_if_fail()
$s_ver.enable_output(x)
$s_ver.set_param(gain, $param)
$s_ver.runfor(10)

$first_t = calc(first($s_ver.traces.t))
$first_x = calc(first($s_ver.traces.x))
$last_t = calc(last($s_ver.traces.t))
$last_x = calc(last($s_ver.traces.x))
assert($first_t == 0)
assert($first_x == 0)
assert($last_t == 10)
assert($last_x == 20)

$s_ver.traces.x
$x_ver = calc($s_ver.traces.x)

info(Check to make sure that traces are counting evenly)
assert(all(diff(diff(($s_ver.traces.t)))==0))
assert(all(diff($x_ver) == $param))

# now run for some longer
$s_ver.set_param(gain, 3)
$s_ver.runfor(10)
$s_ver.finish()
$s_ver.traces

info(Now run again with a higher gain)
assert(all(diff(diff(($s_ver.traces.t)))==0))
$last_x_ver = calc(last($s_ver.traces.x))
assert($last_x_ver == 50)

$s_luts = compile(../models/counter.dsl, luts)
$s_luts.enable_output(x)
$s_luts.set_param(gain, 2)
$s_luts.runfor(10)
$s_luts.set_param(gain, 3)
$s_luts.runfor(10)

$last_x_luts = calc(last($s_luts.traces.x))
assert($last_x_luts == 50)
compare($s_ver, $s_luts)

assertion_report
