% ROSSLER attractor demo model in simEngine
%
% Copyright 2010 Simatra Modeling Technologies
%
function rossler

% Simulate the rossler model using simEngine, set the stop time to 1000
o = simex(create_rossler, 1000);

% Create four subplots, the first three being 2D and the final one
% being a 3D plot
subplot(2,2,1)
simplot(o.x(:,2),o.y(:,2))
title('Rossler x vs. y')
subplot(2,2,2)
simplot(o.y(:,2),o.z(:,2))
title('Rossler y vs. z')
subplot(2,2,3)
simplot(o.x(:,2),o.z(:,2))
title('Rossler x vs. z')
subplot(2,2,4)
plot3(o.x(:,2),o.y(:,2),o.z(:,2))
title('Rossler x vs. y vs. z')

end

function m = create_rossler

% Create an iterator (solver) for the Rossler model with increased accuracy
t = Iterator('continuous', 'solver', 'ode45');
t.reltol = 1e-8;
t.abstol = 1e-8;

% Create the Rossler model object
m = Model('rossler', t);

% Define three scalar input parameters to the system
a = m.input('a', 0.2);
b = m.input('b', 0.2);
c = m.input('c', 5.7);

% Define each of the three state variables with their initial values equal
% to zero
x = m.state(0);
y = m.state(0);
z = m.state(0);

% Define each of the three differential equations
m.diffequ(x, -y - z);
m.diffequ(y, x + a * y);
m.diffequ(z, b + z * (x - c));

% Create three outputs for each variable
m.output(x);
m.output(y);
m.output(z);
end