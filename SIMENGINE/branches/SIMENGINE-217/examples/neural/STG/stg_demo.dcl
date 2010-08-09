# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Tue Feb 17 10:16:37 -0500 2009
# ========================================================================

$settings.hardware.gapscale = 0.25

$values = [4,6,8,10]
#$p = plot()
foreach be in (sw, stacked_qa)
  $time = 0
  foreach v in ($values)
    $s = compile(stg_streaming.dsl, $be)
    $s.enable_output(Vm)
    $s.set_param(gCaS, $v)
    $t = tic()
    $s.runfor(1000)
    #plot($s)
    #$p.add_trace($s, gCaS=$v)
    #$p.plot()
    $s.finish()
    $run_time = $t.toc()
    $time = calc($time + $run_time)
  end
  info(Running stg_streaming.dsl on back-end $be)
  metric($time)
end

foreach be in (sw, stacked_qa)
  $time = 0
  foreach v in ($values)
    $s = compile(stg_events.dsl, $be)
    $s.enable_output(Vm)
    $s.set_param(gCaS, $v)
    $t = tic()
    $s.runfor(11000)
    #plot($s)
    #$p.add_trace($s, gCaS=$v)
    #$p.plot()
    $s.finish()
    $run_time = $t.toc()
    $time = calc($time + $run_time)
  end
  info(Running stg_events.dsl on back-end $be)
  metric($time)
end

assertion_report
