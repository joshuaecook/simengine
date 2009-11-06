# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Sun Oct 19 18:48:16 -0400 2008
# ========================================================================

$dslfile = ../models/fn_model.dsl

foreach be in ($targets)
  $s = compile($dslfile, $be)
  success($s)
  $last.close_if_fail()
end

assertion_report
exit


$s = compile($dslfile, sw)
success($s)
$last.close_if_fail()
$s = compile($dslfile, float)
success($s)
$last.close_if_fail()
$s = compile($dslfile, qc)
success($s)
$s = compile($dslfile, qcs)
success($s)
$s = compile($dslfile, noluts)
success($s)
$s = compile($dslfile, luts)
success($s)
$s = compile($dslfile, ver)
success($s)
$last.close_if_fail()
$s = compile($dslfile, hw)
success($s)
$last.close_if_fail()

assertion_report
