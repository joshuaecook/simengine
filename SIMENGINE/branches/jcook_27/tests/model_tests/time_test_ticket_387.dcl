# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Sun Oct 19 09:46:00 -0400 2008
# ========================================================================

$s = compile(../models/fn_model.dsl, sw)
$s.enable_output(u)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 25)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
$s.runfor(50)
assert($last_time == 50)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 100)


$s = compile(../models/fn_model.dsl, float)
$s.enable_output(u)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 25)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
$s.runfor(50)
assert($last_time == 50)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 100)


$s = compile(../models/fn_model.dsl, luts)
$s.enable_output(u)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 25)
$s.runfor(25)
$s.flush_data()
$last_time = calc(last($s.traces.t))
$s.runfor(50)
assert($last_time == 50)
$s.flush_data()
$last_time = calc(last($s.traces.t))
assert($last_time == 100)


assertion_report
