% BOUNCING_BALL  show the trajectory of a bouncing ball as computed 
%  in simEngine
function bouncing_ball

% Execute simEngine
o = simex('bouncing_ball.dsl', 20);

x = o.position(1,2);
y = o.position(1,3);
p = plot(x, y, 'o');
set(p, 'MarkerSize', 15);
set(p, 'MarkerFaceColor', 'r');
axis([0 10 0 15]); % set boundaries
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
