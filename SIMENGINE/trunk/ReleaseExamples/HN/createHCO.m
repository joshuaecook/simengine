% Leech heartbeat timing network model (timing network model)
% From Hill et al, 2001, J. Comp. Neuro
% Copyright 2007-2010 Simatra Modeling Technolgies
% Additional Info at (http://calabreselx.biology.emory.edu/INTRO/INDEX.HTML)


function m = createHCO(t)

if nargin == 0
  % Create a temporal iterator if one is not passed into the function
  t = Iterator('t', 'continuous', 'solver', 'ode23');
end

% Instantiate the hco model
m = Model('hco', t);

% Create the inputs
stimR4 = m.input('stimR4', 0);
Eleak = m.input('Eleak', -62.5);
gleak = m.input('gleak', 11);

% Add HN34 cell submodels
HNL3 = m.submodel(createHN34(t));
HNR3 = m.submodel(createHN34(t));

HNL3.Eleak = Eleak;
HNL3.gleak = gleak;

HNR3.Eleak = Eleak;
HNR3.gleak = gleak;

% Add synapse submodels
synapseR3L3 = m.submodel(createSynapse(t));
synapseL3R3 = m.submodel(createSynapse(t));

synapseR3L3.Vpre = HNR3.Vm;
synapseR3L3.Vpost = HNL3.Vm;
synapseR3L3.gSyn = 60;

synapseL3R3.Vpre = HNL3.Vm;
synapseL3R3.Vpost = HNR3.Vm;
synapseL3R3.gSyn = 60;

HNL3.ISyn = synapseR3L3.ISyn;
HNR3.ISyn = synapseL3R3.ISyn;

HNR3.IStim = stimR4;

% Create outputs
m.output('VmL3', HNL3.Vm);
m.output('VmR3', HNR3.Vm);

end
