#A simple protocol to demonstrate the pBc half-center oscillator.
#Copyright 2007 Simatra Modeling Technologies.

$s = compile(pbc_hco.dsl, sw)

$s.enable_output(*.Vm?)
#set Iext to 15 for 2 seconds
$s.set_param(nrn1.Iext, 15)
$s.runfor(2)
#set Iext to 15 for 23 seconds
$s.set_param(nrn2.Iext, 15)
$s.runfor(23)
$s.finish()

#plot(s.traces.Vm1, s.traces.Vm2)
plot(s.traces.nrn1.Vm, s.traces.nrn2.Vm)

#$s.save_tabular(output.tab)
