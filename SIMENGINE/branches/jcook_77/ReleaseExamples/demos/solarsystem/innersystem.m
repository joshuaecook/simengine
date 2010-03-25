function o = innersystem
o = simex('innersystem.dsl', 5e7);

%paddle_length = 6;
%paddle_width = 2;
%half = paddle_length/2;
%x_p1 = 3;
%x_p2 = 97;

dims = [-2.5e8 -2.5e8 5e8 5e8];

axis off;

rectangle('Position', dims, 'FaceColor', 'Black');

%ball = @(x,y)([x-1 y-1 200/100 60/100]);

%paddle1 = [x_p1-1 0 paddle_width paddle_length];
%paddle2 = [x_p2-1 0 paddle_width paddle_length];



for i=1:length(o.sundata(:,1))
    pause(0.002)
    
    % Recreate background
    clf % clear the figure
    rectangle('Position', dims, 'FaceColor', 'Black');
%    line([50 50], [0 30], 'LineStyle', '--', 'LineWidth', 10, 'Color', 'Green');    
    
    % Add player scores
%    text(3,28, num2str(int32(o.scores(i,2))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');
%    text(96,28, num2str(int32(o.scores(i,3))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');    

    % Add count down
%    text(95,1, num2str(floor(o.ball(end,1)-o.ball(i,1))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');
    
    % Add ball
    line(o.sundata(i,2),o.sundata(i,3), 'Marker', '.', 'MarkerSize', 30, 'Color', 'Yellow');
    line(o.earthdata(i,2),o.earthdata(i,3), 'Marker', '.', 'MarkerSize', 3, 'Color', 'Cyan');

    line(o.mercurydata(i,2),o.mercurydata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', 'Red');
    line(o.venusdata(i,2),o.venusdata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', [0.8 0.8 0.8]);
    line(o.lunadata(i,2),o.lunadata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [1 1 1]);

    line(o.marsdata(i,2),o.marsdata(i,3), 'Marker', '.', 'MarkerSize', 2, 'Color', 'Red');
    line(o.phobosdata(i,2),o.phobosdata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [0.7 0.7 0.7]);
    line(o.phobosdata(i,2),o.deimosdata(i,3), 'Marker', '.', 'MarkerSize', 1, 'Color', [0.7 0.7 0.7]);

    axis([-2.5e8 2.5e8 -2.5e8 2.5e8])
%     axis([0 2.5e8 0 2.5e8])
    axis off
    
    % draw paddle 1
%    paddle1(2) = o.paddle1(i,2)-half;
%    rectangle('Position', paddle1, 'FaceColor', 'Green');
%    % draw paddle 2
%    paddle2(2) = o.paddle2(i,2)-half;
%    rectangle('Position', paddle2, 'FaceColor', 'Green');
end
end
