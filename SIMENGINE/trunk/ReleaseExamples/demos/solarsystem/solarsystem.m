% SOLARSYSTEM - Shows orbital paths of planets (and our moon) around the sun
%
% Copyright 2010 Simatra Modeling Technologies

function solarsystem

ss = create_solar_system;

o = simex(ss, 1e8);

simplot(o.moondata(2:end,2:3), o.earthdata(2:end,2:3), o.sundata(2:end,2:3), o.marsdata(2:end,2:3), o.venusdata(2:end,2:3), o.mercurydata(2:end,2:3), o.jupiterdata(2:end,2:3), o.saturndata(2:end,2:3), o.uranusdata(2:end,2:3), o.neptunedata(2:end,2:3));

end

function m = Planet (t)

  m = Model('Planet', t);
  
  mass = m.input('mass');
  externalForceX = m.input('externalForceX', 0);
  externalForceY = m.input('externalForceY', 0);
  initX = m.input('initX');
  initY = m.input('initY');
  initXv = m.input('initXv', 0);
  initYv = m.input('initYv', 0);
  
  x = m.state('x', initX);
  y = m.state('y', initY);
  
  x_vel = m.state('x_vel', initXv);
  y_vel = m.state('y_vel', initYv);
  
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
G = 6.67e-11;

% Create the Model object
m = Model('solarsystem');

t2 = Iterator('t2', 'continuous', 'solver', 'forwardeuler', 'dt', 24*60*60);

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

moon = m.submodel(Planet(m.timeIterator));
moon.mass = 5.9e24;
moon.initX = 0;
moon.initY = AU-moondist;
moon.initXv = earthspeed+moonspeed;

mars = m.submodel(Planet(m.timeIterator));
mars.mass = 6.419e23;
mars.initX = 0;
mars.initY = 1.523*AU;
mars.initXv = 24.08;

jupiter = m.submodel(Planet(m.timeIterator));
jupiter.mass = 1.8986e27;
jupiter.initX = 0;
jupiter.initY = 5.2*AU;
jupiter.initXv = 13.07;

saturn = m.submodel(Planet(m.timeIterator));
saturn.mass = 5.685e26;
saturn.initX = 0;
saturn.initY = 9.58*AU;
saturn.initXv = 9.69;

uranus = m.submodel(Planet(m.timeIterator));
uranus.mass = 8.6810e25;
uranus.initX = 0;
uranus.initY = 19.23*AU;
uranus.initXv = 6.81;

neptune = m.submodel(Planet(m.timeIterator));
neptune.mass = 1.024e26;
neptune.initX = 0;
neptune.initY = 30.1*AU;
neptune.initXv = 5.43;


m.output('sundata', sun.x, sun.y, 'iter', t2);
m.output('earthdata', earth.x, earth.y, 'iter', t2);
m.output('venusdata', venus.x, venus.y, 'iter', t2);
m.output('mercurydata', mercury.x, mercury.y, 'iter', t2);
m.output('moondata', moon.x, moon.y, 'iter', t2);
m.output('marsdata', mars.x, mars.y, 'iter', t2);
m.output('jupiterdata', jupiter.x, jupiter.y, 'iter', t2);
m.output('saturndata', saturn.x, saturn.y, 'iter', t2);
m.output('uranusdata', uranus.x, uranus.y, 'iter', t2);
m.output('neptunedata', neptune.x, neptune.y, 'iter', t2);

m.solver = 'rk4';
m.dt = 15*60; % update every 15 minutes


connect_planets(sun, mercury, venus, earth, moon, mars, jupiter, ...
                saturn, uranus, neptune);

end

function connect_planets (varargin)
  G = 6.67e-11;
  
  planets = varargin;
  
  for i = 1:length(planets)
    externalForceX = 0;
    externalForceY = 0;
    
    for j = 1:length(planets)
      if(i ~= j)
        distanceSqr = (planets{i}.x * 1e3 - planets{j}.x * 1e3)^2 + ...
            (planets{i}.y * 1e3 - planets{j}.y * 1e3)^2;
        
        force = -G * planets{i}.amass * planets{j}.amass / ...
                distanceSqr;
        
        distance = distanceSqr ^ 0.5;
        
        xcoeff = (planets{i}.x * 1e3 - planets{j}.x * 1e3) / distance;
        ycoeff = (planets{i}.y * 1e3 - planets{j}.y * 1e3) / distance;
        
        externalForceX = externalForceX + xcoeff * force;
        externalForceY = externalForceY + ycoeff * force;
      end
    end

    planets{i}.externalForceX = externalForceX;
    planets{i}.externalForceY = externalForceY;
  end
end