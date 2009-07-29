# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Thu Oct 16 14:16:11 -0400 2008
# ========================================================================

foreach be in (stacked_qa)
# Create three simulations of FN
$s1 = compile(fn_model.dsl, $be)
$s1.enable_output(*)
$s1.set_param(I, 1.2283)
$s1.runfor(500)
$s1.finish()

$s2 = compile(fn_model.dsl, $be)
$s2.enable_output(*)
$s2.set_param(I, 1.75)
$s2.runfor(500)
$s2.finish()

$s3 = compile(fn_model.dsl, $be)
$s3.enable_output(*)
$s3.set_param(I, 2.25)
$s3.runfor(500)
$s3.finish()

# Demonstrate the new plotter
plot($s1.traces.u, $s2.traces.u, $s3.traces.u, title = FitzHugh-Nagumo Waveforms for Varying Inputs ($be), xlabel = Time)
plot($s1.traces.u:$s1.traces.w, $s2.traces.u:$s2.traces.w, $s3.traces.u:$s3.traces.w, title = Phase Plane Plots ($be), xlabel = u, ylabel = w)
# Demonstrate saving a plot to a file (only png right now)
plot($s1.traces.u:$s1.traces.w, $s2.traces.u:$s2.traces.w, $s3.traces.u:$s3.traces.w, title = Phase Plane Plots ($be), xlabel = u, ylabel = w, output = fn_phase_plane.png)

end
