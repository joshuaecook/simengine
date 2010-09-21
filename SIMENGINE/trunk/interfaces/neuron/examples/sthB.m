
n = Neuron('sthB');

ndend = 2;

soma = n.section('soma');
dend = n.section('dend', ndend);

soma.nseg = 1;
soma.diam = 18.8;
soma.L = 18.8;
soma.Ra = 123;
soma.insert('hh');
soma.gnabar_hh = 0.25;
soma.gl_hh = 0.0001666;
soma.el_hh = -60;

dend{1}.nseg = 5;
dend{1}.diam = 3.18;
dend{1}.L = 701.9;
dend{1}.Ra = 123;
dend{1}.insert('pas');
dend{1}.g_pas = 0.00016666;
dend{1}.e_pas = -60;

dend{2}.nseg = 5;
dend{2}.diam = 2.0;
dend{2}.L = 549.1;
dend{2}.Ra = 123;
dend{2}.insert('pas');
dend{2}.g_pas = 0.00016666;
dend{2}.e_pas = -60;

n.connect(dend{1}, 0, soma, 0);
n.connect(dend{2}, 0, soma, 1);

stim = n.IClamp(soma, 0.5);
stim.del = 100;
stim.dur = 100;
stim.amp = 0.1;

tstop = 300;
o = n.simex(tstop);

subplot(1,2,1);
simplot(o.v_soma);
title('Somatic Voltage');
subplot(2,2,2);
simplot(o.v_dend1);
title('Dendritic branch #1')
subplot(2,2,4);
simplot(o.v_dend2);
title('Dendritic branch #2')

