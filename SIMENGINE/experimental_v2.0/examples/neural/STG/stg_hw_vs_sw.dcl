# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Tue Feb 17 10:16:37 -0500 2009
# ========================================================================

$settings.hardware.gapscale = 0.25

$values = [4]
$dummyx = [40000, 40000]
$dummyy = [0, 5]
$p = plot(title = STG Model - Varying S-Type Ca2+ Conductances, xlabel = Time (ms), ylabel = Voltage (V))
$p2 = plot(title = STG Model - Varying S-Type Ca2+ Conductances, xlabel = Time (ms), ylabel = Voltage (V))
foreach v in ($values)
  $s = compile(stg_streaming.dsl, sw)
  $h = compile(stg_streaming.dsl, stacked_carl)
  $s.disable_output(*)
  $h.disable_output(*)
  $s.set_param(gCaS, $v)
  $h.set_param(gCaS, $v)
  $s.runfor(40000)
  $h.runfor(40000)
  $s.enable_output(Vm)
  $h.enable_output(Vm)
  $s.enable_output(t)
  $h.enable_output(t)
  $s.runfor(2000)
  $h.runfor(2000)
  $p.add_plot($dummyx:$dummyy, , points)
  $p.add_plot($s, Software)
  $p2.add_plot($h, Hardware)
  # This makes the plot draw even when data has not arrived yet
  $p2.add_plot($dummyx:$dummyy, ,points)
  $p.plot()
  $p2.plot()

  $s.finish()
  $h.finish()

end


