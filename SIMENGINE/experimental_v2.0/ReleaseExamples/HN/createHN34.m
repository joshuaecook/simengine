% Leech heartbeat timing network model (timing network model)
% From Hill et al, 2001, J. Comp. Neuro
% Copyright 2007-2010 Simatra Modeling Technolgies
% Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)

function m = createHN34(t)

if nargin == 0
  % Create a temporal iterator if one is not passed into the function
  t = Iterator('t', 'continuous', 'solver', 'ode23');
end

% Instantiate the hn34 model
m = Model('hn34', t);

% Create the inputs
gh = m.input('gh', 4);
gleak = m.input('gleak', 8);
Eleak = m.input('Eleak', -65);
ISyn = m.input('ISyn', 0);
IStim = m.input('IStim', 0);

% Constants
% Membrane Capacitance
Cmem = 0.499; % pF

%Maximal conductances in nS
gNa = 200;
gP = 7.0;
gCaF = 5.0; 
gCaS = 3.2;
gK1 = 100;
gK2 = 80;
gKA = 80;

ENa = 45; % mV
ECa = 135; 
EK = -70;
Eh = -21;

% States
Vm0 = m.input('Vm0', -45);
Vm = m.state('Vm', Vm0, 'iter', t);
hNa = m.state('hNa', 0.99 , 'iter', t);
mP = m.state('mP', 0.32 , 'iter', t);
mCaS = m.state('mCaS', 0.04 , 'iter', t);
hCaS = m.state('hCaS', 0.78 , 'iter', t);
mCaF = m.state('mCaF', 0.02 , 'iter', t);
hCaF = m.state('hCaF', 0.74 , 'iter', t);
mK1 = m.state('mK1', 0.03, 'iter', t);
hK1 = m.state('hK1', 0.81, 'iter', t);
mKA = m.state('mKA', 0.46, 'iter', t);
hKA = m.state('hKA', 0.05, 'iter', t);
mK2 = m.state('mK2', 0.16, 'iter', t);
mh = m.state('mh', 0.08, 'iter', t);

% Equations

% Helper functions
xinf = @(a, b, V)(1/(1 + exp(a * (V + b))));
taux = @(a, b, c, e, V)(c + e / (1 + exp(a * (V + b))));

% Ionic Currents
mNa = xinf(-0.150, 29, Vm);
INa = gNa*mNa^3*hNa*(Vm - ENa);
IP = gP*mP*(Vm - ENa);
ICaF = gCaF*mCaF^2*hCaF*(Vm - ECa);
ICaS = gCaS*mCaS^2*hCaS*(Vm - ECa);
IK1 = gK1*mK1^2*hK1*(Vm - EK);
IK2 = gK2*mK2^2*(Vm - EK);
IKA = gKA*mKA^2*hKA*(Vm - EK);
Ih = gh*mh^2*(Vm - Eh);
Ileak = gleak*(Vm-Eleak);

% Differential Equations
m.diffequ(hNa, (xinf(0.500, 30, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 28))) + 0.01/(cosh(0.3*(Vm + 27)))));
m.diffequ(mP, (xinf(-0.120, 39, Vm) - mP) / taux(0.4, 57, 0.01, 0.2, Vm));
m.diffequ(mCaF, (xinf(-0.6, 46.7, Vm) - mCaF) / (0.011 + 0.024/cosh(0.3*(Vm + 46.7))));
m.diffequ(hCaF, (xinf(0.35, 55.5, Vm) - hCaF) / taux(0.27, 55, 0.06, 0.31, Vm));
m.diffequ(mCaS, (xinf(-0.42, 47.2, Vm) - mCaS) / taux(-0.4, 48.7, 0.005, 0.134, Vm));
m.diffequ(hCaS, (xinf(0.36, 55, Vm) - hCaS) / taux(-0.25, 43, 0.2, 5.25, Vm));
m.diffequ(mK1, (xinf(-0.143, 21, Vm) - mK1) / taux(.150, 16, .001,.011, Vm));
m.diffequ(hK1, (xinf(.111, 28, Vm) - hK1) / taux(-.143, 13, 0.5, 0.2, Vm));
m.diffequ(mK2, (xinf(-0.083, 20, Vm) - mK2) / taux(0.2, 35, 0.057, 0.043, Vm));
m.diffequ(mKA, (xinf(-.13, 44, Vm) - mKA) / taux(0.2, 30, 0.005, 0.011, Vm));
m.diffequ(hKA, (xinf(.16, 63, Vm) - hKA) / taux(-0.3, 55, 0.026, 0.0085, Vm));
m.diffequ(mh, (1/(1 + 2*exp(.180*(Vm + 47)) + exp(0.5*(Vm + 47))) - mh) / taux(-0.1, 73, 0.7, 1.7, Vm));
m.diffequ(Vm, 1/Cmem*(-INa-IP-ICaF-ICaS-IK1-IK2-IKA-Ih-Ileak+IStim-ISyn));

% Output
m.output('Vm');

end
