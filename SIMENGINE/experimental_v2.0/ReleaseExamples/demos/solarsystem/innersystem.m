% INNERSYSTEM - displays planets as circles moving in their orbital paths
%
% Copyright 2010 Simatra Modeling Technologies
function o = innersystem
o = simex('innersystem.dsl', 5e7);

dims = [-2.5e8 -2.5e8 5e8 5e8];

axis off;

rectangle('Position', dims, 'FaceColor', 'Black');

for i=1:length(o.sundata(:,1))
    pause(0.002)
    
    % Recreate background
    clf % clear the figure
    rectangle('Position', dims, 'FaceColor', 'Black');

    % Add planets
    line(o.sundata(i,2),o.sundata(i,3), 'Marker', '.', 'MarkerSize', 30, 'Color', 'Yellow');
    line(o.earthdata(i,2),o.earthdata(i,3), 'Marker', '.', 'MarkerSize', 3, 'Color', 'Cyan');

    line(o.mercurydata(i,2),o.mercurydata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', 'Red');
    line(o.venusdata(i,2),o.venusdata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', [0.8 0.8 0.8]);
    line(o.lunadata(i,2),o.lunadata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [1 1 1]);

    line(o.marsdata(i,2),o.marsdata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', 'Red');
    line(o.phobosdata(i,2),o.phobosdata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [0.7 0.7 0.7]);
    line(o.phobosdata(i,2),o.deimosdata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [0.7 0.7 0.7]);

    axis([-2.5e8 2.5e8 -2.5e8 2.5e8])
    axis off
    
end
end
