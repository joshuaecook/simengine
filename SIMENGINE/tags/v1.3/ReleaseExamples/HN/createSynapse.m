% Leech heartbeat timing network model (timing network model)
% From Hill et al, 2001, J. Comp. Neuro
% Copyright 2007-2010 Simatra Modeling Technolgies
% Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)

function m = createSynapse(t)

if nargin == 0
  % Create a temporal iterator if one is not passed into the function
  t = Iterator('t', 'continuous', 'solver', 'ode23');
end

% Instantiate the hco model
m = Model('synapse', t);

% Create the inputs
Vpre = m.input('Vpre', 0);
Vpost = m.input('Vpost', 0);
gSyn = m.input('gSyn', 60); % nS
tRise = m.input('tRise', 0.002);
tFall = m.input('tFall', 0.011);

% Constant
Vthresh = -20;
ESyn = -62.5;

% States
fSyn = m.state('fSyn', 0, 'iter', t);
MSyn = m.state('MSyn', 0.1, 'iter', t);

% Equations
taux = @(a, b, c, e, V) (c + e / (1 + exp(a * (V + b))));

m.diffequ(fSyn, piecewise((0.999-fSyn)/tRise, Vpre > Vthresh, ...
                          -fSyn/tFall));
m.diffequ(MSyn, (taux(-0.99, 40, 0.1, 0.9, Vpre) - MSyn) / 0.2);

% Outputs
m.output('ISyn', gSyn * fSyn * MSyn * (Vpost - ESyn));

end
