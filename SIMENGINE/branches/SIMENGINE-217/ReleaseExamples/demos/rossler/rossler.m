% ROSSLER attractor demo model in simEngine
%
% Copyright 2010 Simatra Modeling Technologies
%
function rossler

% Simulate the rossler model using simEngine, set the stop time to 1000
o = simex('rossler.dsl', 1000);

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