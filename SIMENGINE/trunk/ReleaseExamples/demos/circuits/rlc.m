% RLC 
%  Simulates a Resistor-Capacitor-Inductor (RLC) series circuit
%
% Copyright 2010 Simatra Modeling Technologies
%
function o = rlc

% Execute the rlc model for just 2.5 seconds
o = simex('rlc.dsl', 2.5);

% Plot both the voltage 
figure(1);
subplot(2,1,1);
simplot(o.V);
title('RLC Series Circuit Voltage')
ylabel('Voltage (V)')
legend('Va','Vb','Vc');

% and the current
subplot(2,1,2);
simplot(o.I);
title('RLC Series Circuit Current');
ylabel('Current (A)')
xlabel('Time (s)')
legend('Vs', 'R1', 'L1', 'C1');

end