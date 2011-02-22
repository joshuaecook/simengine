% Capacitor - create a capacitor element
%
% Model:
%  inputs: V1, V2, C
%  outputs: I
%
% Copyright (c) 2010 Simatra Modeling Technologies
function m = Capacitor(t)

% Create a voltage source and assign it the passed in iterator
m = Model('Capacitor', t);

% Add the voltage node inputs and a default resistance
C = m.input('C', 1e-6); % 1 uF by default
V1 = m.input('V1');
V2 = m.input('V2');

% Compute a dt
t_value = m.equ(Exp(t));
delta_t = t_value-t_value(t-1);
dt = m.equ(piecewise(delta_t, delta_t > 0,...
                     Exp(t.dt)));

% Produce numerical approximations for dVdt                 
dV1dt = m.equ(V1 - V1(t-1));
dV2dt = m.equ(V2 - V2(t-1));
dVdt = (dV2dt-dV1dt)/dt;
I = C*dVdt;

% Define the output
m.output(I);

end