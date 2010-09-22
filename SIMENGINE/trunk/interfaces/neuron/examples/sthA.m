% sthA - Neuron Tutorial A
%
% Based on the NEURON tutorial by Andrew Gillies and David Sterratt
% http://www.anc.ed.ac.uk/school/neuron/tutorial/tutA.html
%
% Adapted for use in simEngine
%
% Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
%
function sthA()

n = Neuron('sthA');

soma = n.section('soma');
soma.nseg = 1;
soma.diam = 18.8;
soma.L = 18.8;
soma.Ra = 123;
soma.insert('hh');

stim = n.IClamp(soma, 0.5);
stim.del = 100;
stim.dur = 100;
stim.amp = 0.1;

tstop = 300;
o = n.simex(tstop);
figure;
simplot(o.v_soma);
title('Somatic Voltage');

end