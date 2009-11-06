# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Wed Oct 22 11:59:40 -0400 2008
# ========================================================================

$param = 2

# using luts
$s_luts = compile(../models/counter.dsl, luts)
success($s_luts)
$last.close_if_fail()
$s_luts.enable_output(x)
$s_luts.set_param(gain, $param)
$s_luts.runfor(10)
$s_luts.finish()

# using ver
$s_ver = compile(../models/counter.dsl, ver)
success($s_ver)
$last.close_if_fail()
$s_ver.enable_output(x)
$s_ver.set_param(gain, $param)
$s_ver.runfor(10)
$s_ver.traces
$s_ver.finish()

$s_luts.traces.x
$s_ver.traces.x
$x_luts = calc($s_luts.traces.x)
$x_ver = calc($s_ver.traces.x)

info(Check to make sure that traces are counting evenly)
assert(all(diff(diff($s_ver.traces.t))==0))
assert(all(diff(diff($x_ver))==0))

info(Verify that luts is equivalent to verilog)
assert(length($x_luts) == length($x_ver))
$last.close_if_fail()
assert(all($x_luts == $x_ver))

assertion_report
