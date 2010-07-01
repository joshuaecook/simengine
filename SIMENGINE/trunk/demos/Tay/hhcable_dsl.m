% HHCABLE_DSL - generates an hh cable model parameterized by the number of
% compartments
%
% Arguments:
%   N - the number of compartments
%
function m = hhcable_dsl(N)

% First define the global iterators
dt = 0.01;
t_imp = Iterator('t_imp', 'solver', 'linearbackwardeuler', 'dt', dt);
t_exp = Iterator('t_exp', 'solver', 'forwardeuler', 'dt', dt);

% Create an hh template
m_hh = hh(t_imp, t_exp);

% Create a new model for the hh cable 
m = Model('hhcable_dsl');

% Need some inputs
Im = m.input('Im', 300);
d = m.input('d', 100);
Ie = m.input('Ie', 0);

% Just setting some coefficients
dx = 0.001; %step length along cable in cm (10 um); 
dx2 = dx^2;
a = 0.0004; %radius in cm (this is 1 um)
R = 10; %Ohms/cm
r = a/2*R;%20e-8; %a/2*R;

sigma=0.00179; %This is the resistance of the medium S/cm

x = ((0:N-1)-N/2)*dx;
Vout = cell(1,N);
for i=1:N
    % soon, this will accept a vectorized notation, but in the meantime,
    % vectors are handled by cell arrays
    Vout{i} = Ie./(4*pi*sigma*sqrt(d^2+x(i).^2));
end

% Instantiate each of the compartments
compartments = cell(1, N);
for i=1:N
    % create the submodel
    compartments{i} = m.submodel(m_hh);
    % assign the VmOut
    compartments{i}.Vout = Vout{i};
end

% Now connect them all together
if N > 1,
    compartments{1}.I_app = r*(compartments{2}.Vm - compartments{1}.Vm)/dx2 + Im;
    for i=2:(N-1)
        compartments{i}.I_app = r*(compartments{i+1}.Vm-2*compartments{i}.Vm+compartments{i-1}.Vm)/dx2;
    end
    compartments{N}.I_app = r*(compartments{N-1}.Vm - compartments{N}.Vm)/dx2;
else
    compartments{1}.I_app = Im;
end

% Output all the voltages
m.output('Vm1', compartments{1}.Vm);

end

function mdl = hh(t_imp, t_exp)

% define the inner cell
mdl = Model('hh');

% define the inputs to the system
I_app = mdl.input('I_app',20);
Vout = mdl.input('Vout');

% define more quantities
cm = 1;
gk  = 25;
gna = 120;
gl = 0.3;
ek  = -104.58;
ena = 78.56;
el  = -49.0;


% define the states
Vin = mdl.state('Vin', -65, 'iter', t_imp); % use the matrix solver for Vin
Vm = Vin-Vout;
h = mdl.state(0.7541, 'iter', t_exp);
m = mdl.state(0.2446, 'iter', t_exp);
n = mdl.state(0.0289, 'iter', t_exp);

% define all the equations
am=-0.1*(Vm+35)./(exp(-.1*(Vm+35))-1);
bm=4*exp((-Vm-60)/(18));
ah=0.07*exp(-.05*(Vm+60));
bh=1./(1+exp(-.1*(Vm+30)));
an=-0.01*(Vm+50)./(exp(-.1*(Vm+50))-1);
bn=0.125*exp(-0.0125*(Vm+60));

ninf  = an./(an+bn);
tninf = 1./(an+bn);

minf  = am./(am+bm);
tminf = 1./(am+bm);
 
hinf  = ah./(ah+bh);
thinf = 1./(ah+bh);

I_Na = gna * m^3 * h * (Vm - ena);
I_K  = gk * n^4 * (Vm - ek);
I_L  = gl * (Vm - el);

I_sum = -(I_Na + I_K + I_L - I_app);

mdl.diffequ(Vin, I_sum / cm);

mdl.diffequ(m, (minf - m) / tminf);
mdl.diffequ(h, (hinf - h) / thinf);
mdl.diffequ(n, (ninf - n) / tninf);

mdl.output('Vm', Vm);
mdl.output('n', n);
mdl.output('h', h);
mdl.output('m', m);

end