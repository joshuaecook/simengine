% bouncing_ball - show the trajectory of a bouncing ball as computed 
%  in simEngine
%
% Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
%
function bouncing_ball

% Define starting position
x0 = 0; y0 = 10; % m
% Define starting velocity
v0 = 25;         % m/s
angle0 = pi/4;   % rad
% Define the dimensions of the room
width = 10;
height = 25;

% Execute simEngine
ball = create_bouncing_ball(x0, y0, v0, angle0, width, height);
o = simex(ball, 20);

x = o.position(1,2);
y = o.position(1,3);
p = plot(x, y, 'o');
set(p, 'MarkerSize', 15);
set(p, 'MarkerFaceColor', 'r');
axis([0 width 0 height]); % set boundaries
l = line(x, y);
set(l, 'LineStyle', ':');

title('Bouncing Ball by simEngine')

for i=2:length(o.position(:,1))
    dt = o.position(i,1)-o.position(i-1,1);
    x = o.position(i,2);
    y = o.position(i,3);
    pause(dt)
    %refreshdata
    set(p, 'XData', x);
    set(p, 'YData', y);
    set(l, 'XData', o.position(1:i,2));
    set(l, 'YData', o.position(1:i,3));
    drawnow
end


end

function m = create_bouncing_ball(x0, y0, velocity0, angle0, wall, ceiling)

% Create the Model object
m = Model('ball');

% Define some useful constants
damping = 0.8;
drag = 0.1;
gravity = -9.8; % m/s^2

% Declare the states variables
x = m.state(x0);
y = m.state(y0);
v_x = m.state(velocity0*cos(angle0));
v_y = m.state(velocity0*sin(angle0));

% Definition differential equations for position
m.diffequ(x, v_x);
m.diffequ(y, v_y);

% Compute the velocity taking into account ...
m.diffequ(v_x, -drag*v_x);           % drag
m.diffequ(v_y, gravity - drag*v_y);  % and gravity

% we want to set boundaries for our world that the ball bounces in if the y
% position exceeds the ceiling, reduce the value
m.update(y, ceiling-(y-ceiling), 'when', y > ceiling);
m.update(y, -y, 'when', y < 0);
m.update(v_y, -v_y*damping, 'when', y < 0 | y > ceiling);

% do the same for the walls at x=0 and x=wall
m.update(x, wall-(x-wall), 'when', x > wall);
m.update(x, -x, 'when', x < 0);
m.update(v_x, -v_x*damping, 'when', x > wall | x < 0);

% output the position
m.output('position', x, y);

% set the integration method
m.solver = 'forwardeuler';
m.dt = 0.01;

end