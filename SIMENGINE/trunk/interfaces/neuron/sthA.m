n = Neuron('sthA');

soma = n.section('soma');
soma.nseg = 1;
soma.diam = 18.8;
soma.L = 18.8;
soma.Ra = 123;
soma.insert('hh');

stim = n.IClamp(soma, 0.025);
stim.del = 100;
stim.dur = 100;
stim.amp = 0.1;

tstop = 300;
o = n.simex(tstop);
simplot(o.v_soma);
title('Somatic Voltage');
