% RLC 
%  Simulates a Resistor-Capacitor-Inductor (RLC) series circuit
%
% Copyright (c) 2010 Simatra Modeling Technologies
%
function o = rlc

% Execute the rlc model for just 2.5 seconds
rlc = create_rlc;
o = rlc.simex(2.5);

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

% Function to create the RLC Model object
function m = create_rlc

% define the Matlab Model object
m = Model('rlc');
% use an implicit method to simulate this circuit
m.solver = 'linearbackwardeuler';
m.dt = 0.0001;

% pull out a value for time from the model
t = m.time;

% define the component values as inputs
R = m.input('R', 1e2);   % Ohms
C = m.input('C', 1e-6);  % Farads
L = m.input('L', 10);    % Henry's

% create an input voltage wave form which will be driven by a voltage
% source
Vinp = piecewise(0, t < 0.1, ...
                 10, t < 1, ...
                 -10, t < 1.9, ...
                 0); % otherwise

% instantiate each of the submodel components (each of these need a time
% iterator - so just pull it from the top level model)
R1 = m.submodel(Resistor(m.timeIterator));
R1.R = R;
C1 = m.submodel(Capacitor(m.timeIterator));
C1.C = C;
L1 = m.submodel(Inductor(m.timeIterator));
L1.L = L;
Vs = m.submodel(VoltageSource(m.timeIterator));
Vs.Vs = Vinp;
Gnd = m.submodel(Ground(m.timeIterator));

% create a node for each connection between components
Va = create_node({{Vs, 2}}, {{R1, 1}});
Vb = create_node({{R1, 2}}, {{C1, 1}});
Vc = create_node({{C1, 2}}, {{L1, 1}});
Vgnd = create_node({{L1,2}}, {{Gnd, 1}, {Vs, 1}});

% define two outputs - voltage and current
m.output('V', Va, Vb, Vc);
m.output('I', Vs.I, R1.I, L1.I, C1.I);

% now, the create node function that will create a state for each node and
% connect up the voltage terminals of each component
    function node = create_node(ins, outs)
        % Provide some capacitance to store charge at a junction
        Cnode = 1e-16;
        
        % Create the node state
        node = m.state(0);
        
        % Loop through the ins and outs
        Isum = Exp(0);
        for i=1:length(ins)
            % Sum up the currents
            component = ins{i}{1};
            Isum = Isum + component.I;
            
            % connect up the circuit
            v_terminal = ['V' num2str(ins{i}{2})];
            component.(v_terminal) = node;
        end
        for i=1:length(outs)
            % Sum up the currents
            component = outs{i}{1};
            Isum = Isum - component.I;
            
            % connect up the circuit
            v_terminal = ['V' num2str(outs{i}{2})];
            component.(v_terminal) = node;
        end
        
        % Create the differential equation
        m.diffequ(node, 1/Cnode * Isum);
    end


end