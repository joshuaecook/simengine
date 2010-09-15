% hh - Hodgkin & Huxley neuron model implemented in simEngine
%
% Hodgkin & Huxley Giant Squid Axon Model (J Physiol, 1952)
%
% Adapted for use with simEngine
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
%
function hh
% create the model
m = create_hh;

% simulate the model
I_app = 0:10:40;
input.I_app = num2cell(I_app);
o = simex(m, 100, input);

% plot the results
simplot(o(:).Vm);
c = cell(1, length(I_app));
for i=1:length(I_app)
    c{i} = ['I_{app} = ' num2str(I_app(i))];
end
legend(c{:});
title('Hodgkin Huxley neural model');
xlabel('Time (ms)');
ylabel('Membrane potential (mV)');

end

function mdl = create_hh

mdl = Model('hh');

% define the inputs to the system
I_app = mdl.input('I_app',20);
g_Na = mdl.input('g_Na',120);
g_K = mdl.input('g_K',36);
g_L = mdl.input('g_L',0.3);

% define more quantities
Cm = 1;
E_Na = 50;
E_K = -77;
E_L = -54.4;

% define the states with their initial values
Vm = mdl.state(-73.494);
h = mdl.state(0.11646);
m = mdl.state(0.044794);
n = mdl.state(0.67952);

% define all the equations
alpha_m = 0.1 * (Vm + 40) / (1 - exp(-(Vm + 40)/(10)));
beta_m  = 4 * exp(-(Vm + 65)/(20));
alpha_h  = 0.07 * exp(-(Vm + 65)/(20));
beta_h   = 1/(1 + exp(-(Vm + 35)/(10)));
alpha_n  = (0.01 * (Vm + 55))/(1 - exp(-(Vm + 55)/(10)));
beta_n   = 0.125 * exp(-(Vm + 65)/(80));

I_Na = g_Na * m^3 * h * (Vm - E_Na);
I_K  = g_K * n^4 * (Vm - E_K);
I_L  = g_L * (Vm - E_L);

I_sum = -(I_Na + I_K + I_L - I_app);

mdl.diffequ(Vm, I_sum / Cm);

m_inf = alpha_m / (alpha_m + beta_m);
m_tau = 1 / (alpha_m + beta_m);

h_inf = alpha_h / (alpha_h + beta_h);
h_tau = 1 / (alpha_h + beta_h);

n_inf = alpha_n / (alpha_n + beta_n);
n_tau = 1 / (alpha_n + beta_n);

mdl.diffequ(m, (m_inf - m) / m_tau);
mdl.diffequ(h, (h_inf - h) / h_tau);
mdl.diffequ(n, (n_inf - n) / n_tau);

mdl.output(Vm);

end
