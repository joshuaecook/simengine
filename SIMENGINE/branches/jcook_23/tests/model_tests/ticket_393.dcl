# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Wed Oct 22 11:59:40 -0400 2008
# ========================================================================

$s = compile(../models/counter.dsl )
success($s)
$last.close_if_fail()
$s.enable_output(x)
$s.set_param(gain, -1.1)
$s.runfor(10)
$s.traces.x
$x = calc($s.traces.x)
info(Defect 393 dealt with a duplication of the number zero in the calculated output)
assert(length(find($x == 0)) == 1)
assertion_report
