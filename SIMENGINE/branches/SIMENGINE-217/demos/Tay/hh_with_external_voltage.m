% hh_with_external_voltages
%   Demonstration of multi-compartment hh cable model running in an
%   embarrassingly parallel simulation.
function hh_with_external_voltage

% Generate the model with N compartments
N = 50;
m = hhcable_dsl_ext_Vout(N);

% Define parameters of the simulation
len = 0.050;
dx = len/N; %step length along cable in cm (10 um); 
sigma=0.00179; %This is the resistance of the medium S/cm

% Determine field potentials
x = ((0:N-1)-N/2)*dx;
Ie = 10;
d = 1:100;
Vout = zeros(N, length(d));
for i=1:length(d)    
    Vout(:,i) = Ie./(4*pi*sigma*sqrt(d(i)^2+x.^2));
end

% Create input structure
s = struct();
for i=1:N
    name = ['Vout' num2str(i)];
    s.(name) = num2cell(Vout(i,:));
end

% Run the simulation...
stop_time = 100;
tic
o = simex(m, stop_time, s, '-optimize=false');
disp(sprintf('Simulation took %g seconds', toc));

% Plot the results
for iter=1:length(d)
    simplot(o(iter).Vms);
    title(['Voltage Trajectories d=' num2str(d(iter))]);
    pause(0.1);
end


end