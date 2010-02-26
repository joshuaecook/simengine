function o = pong
o = simex('pong.dsl', 1000);

disp(sprintf('Score %d vs. %d', o.scores(end,2), o.scores(end,3)));
%return;

paddle_length = 6;
paddle_width = 2;
half = paddle_length/2;
x_p1 = 3;
x_p2 = 97;

figure(1)
axis off;
rectangle('Position', [0 0 100 30], 'FaceColor', 'Black');

ball = @(x,y)([x-1 y-1 200/100 60/100]);
paddle1 = [x_p1-1 0 paddle_width paddle_length];
paddle2 = [x_p2-1 0 paddle_width paddle_length];



for i=1:10:length(o.ball(:,1))
    pause(0.001)
    
    % Recreate background
    clf % clear the figure
    rectangle('Position', [0 0 100 30], 'FaceColor', 'Black');
    line([50 50], [0 30], 'LineStyle', '--', 'LineWidth', 2, 'Color', 'Green');    
    
    % Add player scores
    text(3,28, num2str(int32(o.scores(i,2))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');
    text(96,28, num2str(int32(o.scores(i,3))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');    

    % Add count down
    text(92,1, num2str(floor(o.ball(end,1)-o.ball(i,1))), 'Color', 'Green', 'FontSize', 20, 'FontName', 'Courier');
    
    % Add ball
    line(o.ball(i,2),o.ball(i,3), 'Marker', '.', 'MarkerSize', 20, 'Color', 'Green');
    axis([0 100 0 30])
    axis off
    
    % draw paddle 1
    paddle1(2) = o.paddle1(i,2)-half;
    rectangle('Position', paddle1, 'FaceColor', 'Green');
    % draw paddle 2
    paddle2(2) = o.paddle2(i,2)-half;
    rectangle('Position', paddle2, 'FaceColor', 'Green');
end

% figure(2)
% subplot(1,2,1)
% plot(0,0,'+',cos(o.angle_l(:,2)), sin(o.angle_l(:,2)),'*')
% axis([-1,1,-1,1])
% subplot(1,2,2)
% plot(0,0,'+',cos(o.angle_r(:,2)), sin(o.angle_r(:,2)),'*')
% axis([-1,1,-1,1])

end
