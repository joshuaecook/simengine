function o = rlc

o = simex('rlc.dsl', 2.5);
figure(1);
subplot(2,1,1);
simplot(o.V);
title('RLC Series Circuit Voltage')
ylabel('Voltage (V)')
legend('Va','Vb','Vc');
subplot(2,1,2);
simplot(o.I);
title('RLC Series Circuit Current');
ylabel('Current (A)')
xlabel('Time (s)')


end