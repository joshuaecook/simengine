Tfinal = 100;
Im = 300;
N = 100;
d = 100;
mo = .2446;
no = .0289;
ho = .7541;
vo = -65;
y_init = [vo; no; mo; ho];
Ie = 0;
O=[];
%[t,y] = ode15s(@hh,[0,Tfinal],y_init,O, Im);
%y_init=y(end,:);
y_init = repmat(y_init, N,1);
tic
[T,Y] = ode15s(@hhcable,[0, Tfinal],y_init,O, Im, N, d, Ie);
disp(['Matlab took ' num2str(toc) ' seconds to complete ' num2str(Tfinal) ' ms of simulation'])
dslfile = toDSL(hhcable_dsl(N));
inputs = struct('Im', Im, 'd', d, 'Ie', Ie);
tic
o = simex(dslfile, [0 Tfinal], inputs, '-optimize=false');
disp(['simEngine took ' num2str(toc) ' seconds to complete ' num2str(Tfinal) ' ms of simulation'])
figure,
simplot([T Y(:,1)], o.Vm1);
legend('Matlab', 'simEngine')

