tic
m = buildEngine('timingNetwork.dsl');
buildTime = toc;
tic
s = timingNetwork(50, m.inputs);
simTime = toc;

figure, hold on
plot(s.VmL3(:,1), s.VmL3(:,2), 'r')
plot(s.VmR3(:,1), s.VmR3(:,2)-60, 'b')
plot(s.VmL1(:,1), s.VmL1(:,2)-60*2, 'r')
plot(s.VmR1(:,1), s.VmR1(:,2)-60*3, 'b')
plot(s.VmL4(:,1), s.VmL4(:,2)-60*4, 'r')
plot(s.VmR4(:,1), s.VmL4(:,2)-60*5, 'b')

title([num2str(buildTime) 's to build model, ' num2str(simTime) 's ' ...
                    'to simulate 50 sec.'])
xlabel('Time (s)');
