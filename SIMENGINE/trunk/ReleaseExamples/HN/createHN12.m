% Leech heartbeat timing network model (timing network model)
% From Hill et al, 2001, J. Comp. Neuro
% Copyright 2007-2010 Simatra Modeling Technolgies
% Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)

function m = createHN12(t)

if nargin == 0
  % Create a temporal iterator if one is not passed into the function
  t = Iterator('t', 'continuous', 'solver', 'ode23');
end

% Instantiate the hn34 model
m = Model('hn12', t);

% Inputs
gleak = m.input('gleak', 10);
Eleak = m.input('Eleak', -40);
ISyn = m.input('ISyn', 0);
IStim = m.input('IStim', 0);

% Constants
% Membrane Capacitance
Cmem = 0.5; % pF

% Maximal conductances in nS
gNa = 200;
gK1 = 150;
gK2 = 75;

ENa = 45; % mV
EK = -70;


% States
Vm = m.state('Vm', -45, 'iter', t);
hNa = m.state('hNa', 0.99 , 'iter', t);
mK1 = m.state('mK1', 0.03, 'iter', t);
hK1 = m.state('hK1', 0.81, 'iter', t);
mK2 = m.state('mK2', 0.16, 'iter', t);

% Equations
% Helper functions
xinf = @(a, b, V)(1/(1 + exp(a * (V + b))));
taux = @(a, b, c, e, V)(c + e / (1 + exp(a * (V + b))));

% Ionic Currents
mNa = xinf(-0.150, 29, Vm);
INa = gNa*mNa^3*hNa*(Vm - ENa);
IK1 = gK1*mK1^2*hK1*(Vm - EK);
IK2 = gK2*mK2^2*(Vm - EK);
Ileak = gleak*(Vm-Eleak);

% Differential Equations
m.diffequ(hNa, (xinf(0.500, 30, Vm) - hNa) / (0.004 + 0.006/(1 + exp(0.5*(Vm + 28))) + 0.01/(cosh(0.3*(Vm + 27)))));
m.diffequ(mK1, (xinf(-0.143, 21, Vm) - mK1) / taux(.150, 16, .001,.011, Vm));
m.diffequ(hK1, (xinf(.111, 28, Vm) - hK1) / taux(-.143, 13, 0.5, 0.2, Vm));
m.diffequ(mK2, (xinf(-0.083, 20, Vm) - mK2) / taux(0.2, 35, 0.057, 0.043, Vm));
m.diffequ(Vm, 1/Cmem*(-INa-IK1-IK2-Ileak+IStim-ISyn));

% Output
m.output(Vm);

end
