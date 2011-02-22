function pas(mdl)

mdl.suffix = 'pas';

% define the parameters of pas
e = mdl.range_parameter('e', -70);
g = mdl.range_parameter('g', 0.001);

% define an output current
ipas = g * (mdl.v - e);
mdl.current(ipas);

end