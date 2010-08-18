function m = hh_compartment

% First define the global iterators
dt = 0.01;
t_imp = Iterator('t_imp', 'solver', 'linearbackwardeuler', 'dt', dt);
t_exp = Iterator('t_exp', 'solver', 'forwardeuler', 'dt', dt);

% Choose how many compartments you would like
numCompartments = 100;

% Create an hh template
m_hh = hh(t_imp, t_exp);

m = Model('hh_compartment');

% Need three inputs
I_begin = m.input('I_begin', 30);
I_middle = m.input('I_middle', 0);
I_end = m.input('I_end', 20);

R = 1; % define an axonal resistance

% Instantiate each of the compartments
compartments = cell(1, numCompartments);
for i=1:numCompartments
    compartments{i} = m.submodel(m_hh);
end

% Now connect them all together
compartments{1}.I_app = (compartments{2}.Vm - compartments{1}.Vm)/R + I_begin;
for i=2:(numCompartments-1)
    compartments{i}.I_app = (compartments{i+1}.Vm-compartments{i}.Vm)/R + ...
        (compartments{i-1}.Vm-compartments{i}.Vm)/R + I_middle;
end
compartments{numCompartments}.I_app = (compartments{numCompartments-1}.Vm - compartments{numCompartments}.Vm)/R + I_end;

Vms = cell(1,numCompartments);
for i=1:numCompartments
    Vms{i} = compartments{i}.Vm;
end

% Output all the voltages
%m.output('Vms', Vms);

% Create spike outputs
spikes = cell(1,numCompartments);
for i=1:numCompartments
    Vm = m.equ(compartments{i}.Vm);
    spikes{i} = m.equ((Vm(t_imp-1) > Vm(t_imp-2)) & ...
                      (Vm(t_imp-1) > Vm(t_imp)) & ...
                      (Vm(t_imp-1) > -25));
end

m.output('spikes', spikes, 'when', reduction_or(spikes));


end

function mdl = hh(t_imp, t_exp)

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

% define the states
Vm = mdl.state('Vm', -73.494, 'iter', t_imp);
h = mdl.state(0.11646, 'iter', t_exp);
m = mdl.state(0.044794, 'iter', t_exp);
n = mdl.state(0.67952, 'iter', t_exp);

% define all the equations
alpha_m = 0.1 * (Vm + 40) / (1 - exp(-(Vm + 40)/(10)));
beta_m  = 4 * exp(-(Vm + 65)/(20));
alpha_h  = 0.07 * exp(-(Vm + 65)/(20));
beta_h   = 1/(1 + exp(-(Vm + 35)/(10)));
alpha_n  = (0.01 * (Vm + 55))/(1 - exp(-(Vm + 55)/(10)));
beta_n   = 0.125 * exp(-(Vm + 65)/(80));

I_Na = g_Na * m*m*m * h * (Vm - E_Na);
I_K  = g_K * n*n*n*n * (Vm - E_K);
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

mdl.output('Vm');

end

% Create helper function to reduce a cell array of boolean expressions to
% one expression - replace with the ANY command when vectorized.
function er = reduction_or(cell_list)
if ~isempty(cell_list)
    er = cell_list{1};
    for i=2:length(cell_list)
        er = er | cell_list{i};
    end
else
    er = Exp(1);
end
end
