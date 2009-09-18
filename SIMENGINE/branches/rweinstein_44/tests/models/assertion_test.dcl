# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Fri Oct 17 13:54:43 -0400 2008
# ========================================================================

# Run a bunch of simulations
$s = compile(fn_model.dsl )
success($s)
$last.close_if_fail()
$s.enable_output(*)
$s.runfor(100)
$s.finish()
$s1 = compile(tests/models/fn_model.dsl, float)
$s1.enable_output(*)
$s1.runfor(100)
$s1.finish()
$s2 = compile(tests/models/fn_model.dsl, qc)
$s2.enable_output(*)
$s2.runfor(100)
$s2.finish()
$s3 = compile(tests/models/fn_model.dsl, qcs)
$s3.enable_output(*)
$s3.runfor(100)
$s3.finish()

# Now begin taking metrics and pushing information
info(Comparisons Between Waveforms)
compare($s, $s1)
compare($s, $s2)
compare($s, $s3)
info(Frequency of Spiking)
$s_freq = calc(peakFreq($s.traces.t, $s.traces.u))
$s1_freq = calc(peakFreq($s1.traces.t, $s1.traces.u))
$s2_freq = calc(peakFreq($s2.traces.t, $s2.traces.u))
$s3_freq = calc(peakFreq($s3.traces.t, $s3.traces.u))
assert($s_freq == 0.03992)
assert($s1_freq == 0.03992)
assert($s2_freq == 0.03992)
assert($s3_freq == 0.03992)
$range_u = calc([min($s.traces.u) max($s.traces.u)]);
$range_w = calc([min($s.traces.w) max($s.traces.u)]);
$last_t = calc(last($s.traces.t));
metric($range_u)
metric($range_w)
metric($last_t)

assertion_report


