%
% Booth, Rinzel, & Kiehn Motoneuron Model (J Neurophysiol 78:3371-3384, 1997)
% Publication: Compartmental Model of Vertebrate Motoneurons for Ca2+-Dependent Spiking
%              and Plateau Potentials Under Pharmacological Treatment
%
% Adapted for use with simEngine
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
%
function brk()

% create the model
m = create_brk();

% simulate
input.Iext = 20;
o = simex(m, 200, input);

% plot
simplot(o);
title('Booth, Rinzel, & Kiehn Model');
xlabel('Seconds (ms)');
ylabel('Voltage (mV)');

end


function m = create_brk()

% Define the solver for the model
t = Iterator('continuous', 'solver', 'rk4', 'dt', 0.01);

% Create the model with the defined iterator
m = Model('brk', t);

% Conductances (mS/cm^2)
GNa = 120;
GK_dr = 100;
GCa_NS = 14;
GCa_ND = .03;
GK_CaS = 5;
GK_CaD = 1.1;
GCa_L = 0.33;
gleak = 0.51;

% Static parameters
C = 1;
gc = 0.1;      % coupling conductance (mS/cm^2)
p = 0.1;
Kd = 0.2;      % uM
f = 0.01;      % percent free to bound Ca
alpha = 0.009; % mol/C/um
kca = 2;       % Ca removal rate

% Half Activation voltages in mV, Slopes in MV, Time Constants in milliseconds
Vhm = -35;
Sm = -7.8;
Vhh = -55;
Sh = 7;
Vhn = -28;
Sn = -15;
VhmN = -30;
SmN = -5;
VhhN = -45;
ShN = 5;
VhmL = -40;
SmL = -7;
TaumN = 4;
TauhN = 40;
TaumL = 40;

% Reversal potentials in mV
ENa = 55;
EK = -80;
ECa = 80;
Eleak = -60;
  
% State Variable Declaration
Vs = m.state(-60);
Vd = m.state(-60);
h = m.state(0.9);
n = m.state(0);
mnS = m.state(0);
hnS = m.state(0.9);
mnD = m.state(0);
hnD = m.state(0.9);
ml = m.state(0);
CaS = m.state(0);
CaD = m.state(0);

% External Current
Iext = m.input('Iext', 0);
  
% Steady state values
Tauh = 30/(exp((Vs+50)/15)+exp(-(Vs+50)/16));
Taun = 7/(exp((Vs+40)/40)+exp(-(Vs+40)/50));
minf = 1/(1+exp((Vs-Vhm)/Sm));
hinf = 1/(1+exp((Vs-Vhh)/Sh));
ninf = 1/(1+exp((Vs-Vhn)/Sn));
mnSinf = 1/(1+exp((Vs-VhmN)/SmN));
hnSinf = 1/(1+exp((Vs-VhhN)/ShN));
mnDinf = 1/(1+exp((Vd-VhmN)/SmN));
hnDinf = 1/(1+exp((Vd-VhhN)/ShN));
mlinf = 1/(1+exp((Vd-VhmL)/SmL));
    
% Current values
INaS = GNa*minf^3*h*(Vs-ENa);
IKS = (GK_dr*n^4 + GK_CaS*CaS/(CaS+Kd))*(Vs-EK);
ICaS = GCa_NS*mnS^2*hnS* (Vs-ECa);
IleakS = gleak*(Vs-Eleak);
IcouplingS = gc/p*(Vs-Vd);
IKD = GK_CaD*CaD/(CaD+Kd)*(Vd-EK);
ICaD = (GCa_ND*mnD^2*hnD+GCa_L*ml)*(Vd-ECa);
IleakD = gleak*(Vd-Eleak);
IcouplingD = gc/(1-p)*(Vd-Vs);

% Differential equations
m.diffequ(h, (hinf-h)/Tauh);
m.diffequ(n, (ninf-n)/Taun);
m.diffequ(mnS, (mnSinf-mnS)/TaumN);
m.diffequ(hnS, (hnSinf-hnS)/TauhN);
m.diffequ(mnD, (mnDinf-mnD)/TaumN);
m.diffequ(hnD, (hnDinf-hnD)/TauhN);
m.diffequ(ml, (mlinf-ml)/TaumL);
m.diffequ(CaS, f*(-alpha*ICaS-kca*CaS));
m.diffequ(CaD, f*(-alpha*ICaD-kca*CaD));
m.diffequ(Vs, 1/C*(Iext-INaS-IKS-ICaS-IleakS-IcouplingS));
m.diffequ(Vd, 1/C*(-IKD-ICaD-IleakD-IcouplingD));

% Output definition
m.output('V', Vs, Vd);

end