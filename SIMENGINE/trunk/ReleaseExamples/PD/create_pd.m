% create_pd - return a model object for the Pyloric Dilator STG
%
% Lobster STG pacing neuron model
% Derived from Prinz et al, J Neurophysiol, December 2003
% Copyright 2008-2010 Simatra Modeling Technolgies, L.L.C.
%
function m = create_pd(output_Vm)

if nargin == 0
  output_Vm = true;
end

dt = 0.05;
t_expeuler = Iterator('t_ee', 'solver', 'exponentialeuler', 'dt', dt); % this will be the default iterator
m = Model('pd');
m.solver = 'forwardeuler';
m.dt = dt;

% Membrane properties
Amem = 0.6283e-3;
Cmem = 0.0006283;

% Maximal Conductances
gNa = m.input('gNa', 100);
gCaT = m.input('gCaT', 0);
gCaS = m.input('gCaS', 10);
gA = m.input('gA', 40);
gKCa = m.input('gKCa', 25);
gKd = m.input('gKd', 75);
gh = m.input('gh', 0.02);
gleak = m.input('gleak', 0.03);

% More constants
ENa = 50;
EK = -80;
Eh = -20;
Eleak = -50;
Caconc_rest = 0.05;
Tau_Ca = 200;

% State Variable Declaration
Vm = m.state('Vm', -57.5, 'iter', t_expeuler);
mNa = m.state(0.002);
hNa = m.state(0.856);
mCaT = m.state(0.013);
hCaT = m.state(0.991);
mCaS = m.state(0.042);
hCaS = m.state(0.396);
mA = m.state(0.027);
hA = m.state(0.571);
mKCa = m.state(0.027);
mKd = m.state(0.02);
mh = m.state(0.031);
Caconc = m.state('Caconc', 0.05, 'iter', t_expeuler);

% Define all the equations

% Start with some helper functions
xinf = @(a,b,V)(1/(1 + exp((V + a)/b)));
taux = @(a,b,c,e,V)(c + e / (1 + exp((V + a)/b)));

% Nernst equation to calculate Ca2+ reversal
ECa = 29.55*log(3000/Caconc);

INa = gNa*mNa^3*hNa*(Vm - ENa)*Amem;
ICaT = gCaT*mCaT^3*hCaT*(Vm - ECa)*Amem;
ICaS = gCaS*mCaS^3*hCaS*(Vm - ECa)*Amem;
IA = gA*mA^3*hA*(Vm-EK)*Amem;
IKCa = gKCa*mKCa^4*(Vm-EK)*Amem;
IKd = gKd*mKd^4*(Vm-EK)*Amem;
Ih = gh*mh*(Vm - Eh)*Amem;
Ileak = gleak*(Vm-Eleak)*Amem;

m.diffequ(Caconc, (1/200)*(-14960*(ICaT + ICaS) - Caconc + 0.05));
m.diffequ(mNa, (xinf(25.5, -5.29, Vm) - mNa)/(taux(120, -25, 2.64, -2.52, Vm)));
m.diffequ(hNa, (xinf(48.9, 5.18, Vm) - hNa)/(taux(62.9, -10, 0, 1.34, Vm)*taux(34.9, 3.6, 1.5, 1, Vm)));
m.diffequ(mCaT, (xinf(27.1, -7.2, Vm) - mCaT)/(taux(68.1, -20.5, 43.4, -42.6, Vm)));
m.diffequ(hCaT, (xinf(32.1, 5.5, Vm) - hCaT)/(taux(55, -16.9, 210, -179.6, Vm)));
m.diffequ(mCaS, (xinf(33, -8.1, Vm) - mCaS)/(2.8 + 14/(exp((Vm +27)/10) + exp((Vm + 70)/-13))));
m.diffequ(hCaS, (xinf(60, 6.2, Vm) - hCaS)/(120 + 300/(exp((Vm + 55)/9) + exp((Vm+65)/-16))));
m.diffequ(mA, (xinf(27.2, -8.7, Vm) - mA)/(taux(32.9, -15.2, 23.2, -20.8, Vm)));
m.diffequ(hA, (xinf(56.9, 4.9, Vm) - hA)/(taux(38.9, -26.5, 77.2, -58.4, Vm)));
m.diffequ(mKCa, (((Caconc/1)/((Caconc/1) + 3))*xinf(28.3, -12.6, Vm) - mKCa)/(taux(46, -22.7, 180.6, -150.2, Vm)));
m.update(mKCa, 0, 'when', mKCa < 0);
m.diffequ(mKd, (xinf(12.3, -11.8, Vm) - mKd)/(taux(28.3, -19.2, 14.4, -12.8, Vm)));
m.diffequ(mh, (xinf(75, 5.5, Vm) - mh)/(2/(exp((Vm + 169.7)/-11.6) + exp((Vm - 26.7)/14.3))));
m.diffequ(Vm, (1/Cmem)*(-INa-ICaT-ICaS-IA-IKCa-IKd-Ih-Ileak));

% Don't begin outputting data immediately - instead, allow model to settle
start_data_time = m.input('start_data_time', 10000);

if output_Vm
  m.output('Vm', Vm, 'when', m.time > start_data_time);
end

end
