# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Sat Oct 18 15:48:49 -0400 2008
# ========================================================================

# first execute the compiler and close if it didn't run properly
$s = compile(../models/fn_model.dsl, sw)
success($s)
$last.close_if_fail()

$s.enable_output(*)
$s.set_param(I,1.5)
$s.runfor(100)
$s.finish()
info(First check the time to make sure that it begins at 0 and ends at 100)
$first_t = calc(first($s.traces.t))
$last_t = calc(last($s.traces.t))
assert($first_t == 0)
assert($last_t == 100) # error is part of ticket #387

# Run for three input currents, make sure that it is oscillating each time at roughly the same frequency
Info(Run for three input currents, verify oscillation)
$freq_15 = calc(peakFreq($s.traces.t, $s.traces.u))
assert(withinPercent($freq_15, 0.04, 5))
$range_u_15 = calc([min($s.traces.u) max($s.traces.u)])
$range_w_15 = calc([min($s.traces.w) max($s.traces.w)])
$mean_u_15 = calc(mean($s.traces.u))
$u_15 = $s.traces.u;

# repeat for two more input currents
$s.reset()
$s.set_param(I,2.0)
$s.runfor(100)
$s.finish()
$freq_20 = calc(peakFreq($s.traces.t, $s.traces.u))
assert(withinPercent($freq_20, 0.04, 5))
$range_u_20 = calc([min($s.traces.u) max($s.traces.u)])
$range_w_20 = calc([min($s.traces.w) max($s.traces.w)])
$mean_u_20 = calc(mean($s.traces.u))
$u_20 = $s.traces.u;

$s.reset()
$s.set_param(I,2.5)
$s.runfor(100)
$s.finish()
$freq_25 = calc(peakFreq($s.traces.t, $s.traces.u))
assert(withinPercent($freq_25, 0.04, 5))
$range_u_25 = calc([min($s.traces.u) max($s.traces.u)])
$range_w_25 = calc([min($s.traces.w) max($s.traces.w)])
$mean_u_25 = calc(mean($s.traces.u))
$u_25 = $s.traces.u;

plot($u_15, $u_20, $u_25)

info(Verify that with an increased I, the average value keeps on increasing)
assert(all(diff([$mean_u_15 $mean_u_20 $mean_u_25])>0))

assertion_report
