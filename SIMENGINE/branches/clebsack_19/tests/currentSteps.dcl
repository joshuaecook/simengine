# This is an example dcl file for fn.dyn
# A series of increasing current steps.
# Copyright 2007 Simatra Modeling Technologies

$s = compile(tests/fn_model.dsl, sw)

$s.outputs.u = on
$s.outputs.w = on

$s.params.I = 1.6
$s.runfor(50)
$s.params.I = 2.0
$s.runfor(50)
$s.params.I = 2.4
$s.runfor(50)
$s.params.I = 2.8
$s.runfor(50)
$s.params.I = 3.2
$s.runfor(50)
$s.finish()

plot(s.traces.u, s.traces.w)
#$s.save_tabular(output.tab)
