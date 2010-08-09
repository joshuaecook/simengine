n = Neuron('sthA');

soma = n.section('soma');
soma.nseg = 20;
soma.diam = 18.8;
soma.L = 18.8*20;
soma.Ra = 123;
soma.insert('hh');
%soma.insert('pas');

stim = n.IClamp(soma, 0.025);
stim.del = 100;
stim.dur = 100;
%stim.amp = 0.1;
stim.del = 30;
stim.dur = 20;
stim.amp = 1;

tstop = 50;
% [o, yf1] = n.simex(0.5);
% [o, yf2] = n.simex(1);
% [o, yf3] = n.simex(1.5);
% [o, yf4] = n.simex(2);
% [yf1; yf2; yf3; yf4]
o = n.simex(tstop);
simplot(o.v_soma);
title('Somatic Voltage');
