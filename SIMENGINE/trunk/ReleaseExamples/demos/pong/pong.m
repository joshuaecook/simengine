% PONG
%  Simulates a game of pong using simEngine and then visualizing the
%  results in Matlab
%
% Copyright 2010 Simatra Modeling Technologies 
%
function o = pong

% Execute simEngine and save the results to output structure 'o'
o = simex('pong.dsl', 300);

% After finishing, show the final score of the game
disp(sprintf('Score %d vs. %d', o.scores(end,2), o.scores(end,3)));

% Some constants shared in the DSL file and in this function for placement
% of elements
paddle_length = 6;
paddle_width = 2;
half = paddle_length/2;
x_p1 = 3;
x_p2 = 97;
x_size = 100;
y_size = 30;

% Draw to the same figure, turning off the axis and setting a black
% background
figure(1)
axis off;
rectangle('Position', [0 0 x_size y_size], 'FaceColor', 'Black');

% Define the initial positions of the two paddles
paddle1 = [x_p1-1 0 paddle_width paddle_length];
paddle2 = [x_p2-1 0 paddle_width paddle_length];


% Look at every 10th output to make it run faster
for i=1:10:length(o.ball(:,1))
    
    % Allow time for the screen to be refreshed
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

end
