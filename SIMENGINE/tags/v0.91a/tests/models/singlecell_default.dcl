#Protocol to demonstrate the single-cell bursting
#in pBc reduced neuron model.
#Copyright 2007 Simatra Modeling Technologies.

$s = compile(pbc_single.dsl, sw)

$s.enable_output(Vm)
$s.set_param(Iext, 15)
$s.runfor(25)
$s.finish()
plot(s.traces.Vm)
#$s.save_tabular(output.tab)
