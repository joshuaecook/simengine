% SOLARSYSTEM - Shows orbital paths of planets (and our moon) around the sun
%
% Copyright 2010 Simatra Modeling Technologies
% function solarsystem
%   o = simex('solarsystem.dsl', 1e8);
%   simplot(o.moondata(2:end,2:3), o.earthdata(2:end,2:3), o.sundata(2:end,2:3), o.marsdata(2:end,2:3), o.venusdata(2:end,2:3), o.mercurydata(2:end,2:3), o.jupiterdata(2:end,2:3), o.saturndata(2:end,2:3), o.uranusdata(2:end,2:3), o.neptunedata(2:end,2:3));
% end

function solarsystem

ss = create_solar_system

type(ss)

o = simex(ss, 1e8)

%   simplot(o.moondata(2:end,2:3), o.earthdata(2:end,2:3), o.sundata(2:end,2:3), o.marsdata(2:end,2:3), o.venusdata(2:end,2:3), o.mercurydata(2:end,2:3), o.jupiterdata(2:end,2:3), o.saturndata(2:end,2:3), o.uranusdata(2:end,2:3), o.neptunedata(2:end,2:3));

simplot(o.earthdata(2:end, 2:3), o.mercurydata(2:end, 2:3), ...
        o.venusdata(2:end, 2:3));

end

function m = Planet (t)

  m = Model('Planet', t);
  
  mass = m.input('mass');
  externalForceX = m.input('externalForceX');
  externalForceY = m.input('externalForceY');
  initX = m.input('initX');
  initY = m.input('initY');
  initXv = m.input('initXv', 0);
  initYv = m.input('initYv', 0);
  
  x = m.state(initX);
  y = m.state(initY);
  
  x_vel = m.state(initXv);
  y_vel = m.state(initYv);
  
  m.diffequ(x, x_vel);
  m.diffequ(y, y_vel);
  
  m.diffequ(x_vel, externalForceX / mass / 1e3);
  m.diffequ(y_vel, externalForceY / mass / 1e3);

  m.output('x', x);
  m.output('y', y);
  m.output('amass', mass);
  m.output('x_vel', x_vel);
  m.output('y_vel', y_vel);
end


%model (x,y,amass,x_vel, y_vel) = planet (mass, externalForceX, externalForceY, initX, initY, initXv, initYv)


function m = create_solar_system
G = 6.67e-11

% Create the Model object
m = Model('solarsystem');

% all numbers taken from Wikipedia, mass in kg, velocity in km/s
AU = 1.52e8; %km
moondist = 3.84e5; %km
earthspeed = 29.783; %km/s
moonspeed = 1.022; %km/s

sun = m.submodel(Planet(m.timeIterator));
sun.mass = 1.989e30;
sun.initX = 0;
sun.initY = 0;

mercury = m.submodel(Planet(m.timeIterator));
mercury.mass = 3.3e23;
mercury.initX = 0;
mercury.initY = 0.387 * AU;
mercury.initXv = 47.87;

venus = m.submodel(Planet(m.timeIterator));
venus.mass = 4.868e24;
venus.initX = 0;
venus.initY = 0.723 * AU;
venus.initXv = 35.02;

earth = m.submodel(Planet(m.timeIterator));
earth.mass = 5.9e24;
earth.initX = 0;
earth.initY = AU;
earth.initXv = earthspeed;

m.output('earthdata', earth.x, earth.y);
m.output('venusdata', venus.x, venus.y);
m.output('mercurydata', mercury.x, mercury.y);

m.solver = 'rk4';
%m.dt = 15*60; % update every 15 minutes

end

% function connectPlanets (planets)
%   ;
% end