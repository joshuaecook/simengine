% vdp - Van der Pol model
%
% Copyright 2009-2010 Simatra Modeling Technologies, L.L.C.
function vdp

% simulate the Van der Pol model
out = simex(create_vdp, 500);

% plot the results
simplot(out)
title('Van der Pol oscillator')

end

function m = create_vdp

% create a model object
m = Model('vdp');

% the Van der Pol model is one 2nd order differential equation,
% which we're going to separate into two first order differential
% equations
y = m.state(2);
y1 = m.state(0);

m.diffequ(y, y1);
m.diffequ(y1, 100*(1-y^2)*y1-y);

% define the output of the model as y
m.output(y)

end