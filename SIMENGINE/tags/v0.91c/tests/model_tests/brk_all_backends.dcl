# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Sun Oct 19 18:48:16 -0400 2008
# ========================================================================

foreach be in ($targets)
  info(Running brk model on backend '$be')
  $s = compile(../models/brk_model.dsl, $be)
  success($s)
  $last.close_if_fail()
end

assertion_report

