model (pos) = pong_paddle(ball_x, ball_y, approaching)
    
    constant MAX = 30
    constant WIDTH = 6
    constant HALF = WIDTH/2
    constant MAX_SPEED = 10

    state pos = MAX/2
    state v = 1
    equation pos' = v
    equation v' = 0
    //equation v = -v when flip
//    equation v = 0 when ball_x
    equation v = 0 when not(approaching)
    equation v = (ball_y - pos) when approaching

    // set maximums
    equation v = MAX_SPEED when v > MAX_SPEED
    equation v = -MAX_SPEED when v < -MAX_SPEED

    // let it bounce off the walls
    equation v = 0 when pos + HALF >= MAX
    equation v = 0 when pos - HALF <= 0

    // set maximums for the position
    equation pos = HALF when pos < HALF
    equation pos = MAX-HALF when pos > MAX-HALF

end

model (x, y, angle_v) = pong_ball(max_x, max_y, impact_l, impact_r, reset, divert_angle)

    constant SPEED = 10

    random mag with {normal, mean=SPEED, stddev=1}
    random ang with {uniform, low=0, high=pi}
    equation angle = {ang + 0 when ang <= pi/4, 
		      ang + pi/2 when ang <= pi/2,
		      ang + pi/2 when ang <= 3*pi/4,
		      ang + pi otherwise}

    state x = 50
    state y = 15
    state v_x = 0.1
    state v_y = 0.1

    equation x' = v_x
    equation y' = v_y
    
    equation v_x' = 0
    equation v_y' = 0

    // update equations
    equation angle_v = atan2(v_y,v_x)
    equation mag_v = sqrt(v_x^2 + v_y^2)
    equation v_x = -mag_v * cos(angle_v+divert_angle) when impact_l or impact_r
    equation v_y = mag_v * sin(angle_v+divert_angle) when impact_l or impact_r
    equation v_x = 0 when x < 0 or x > max_x
    equation v_y = 0 when x < 0 or x > max_x
    equation v_y = -v_y when y <= 0 or y >= max_y

    // handle the reset case after a score
    equation v_x = mag*cos(angle) when reset
    equation v_y = mag*sin(angle) when reset
    equation x = 50 when reset
    equation y = 15 when reset

end


model (paddle1, paddle2, ball, scores, reset, angle_l, angle_r) = pong()

    constant X_SIZE = 100
    constant Y_SIZE = 30
    constant X_P1 = 3
    constant X_P2 = 97
    constant WIDTH = 6
    constant HALF = WIDTH/2

    // define scores
    state player1 = 0
    state player2 = 0

    // define the ball first
    submodel pong_ball b with {max_x = X_SIZE, max_y = Y_SIZE}
    output ball = (b.x, b.y)
    equation x = b.x
    equation y = b.y
    equation dx = x-x[t[-1]]

    // define a paddle
    submodel pong_paddle p1 with {ball_x = b.x, ball_y = b.y, approaching= dx < 0}
    submodel pong_paddle p2 with {ball_x = b.x, ball_y = b.y, approaching= dx > 0}
    output paddle1 = p1.pos
    output paddle2 = p2.pos    

    // determine impacts and divert angle
    equations
	impact_l = b.x <= (X_P1+1) and (p1.pos+HALF >= b.y and p1.pos-HALF <= b.y)
	impact_r = b.x >= (X_P2-1) and (p2.pos+HALF >= b.y and p2.pos-HALF <= b.y)
	divert_angle = {-(b.x - p1.pos)*pi/16 when impact_l,
			-(b.x - p2.pos)*pi/16 when impact_r,
			0 otherwise}
    end    

    b.impact_l = impact_l
    b.impact_r = impact_r
    b.divert_angle = divert_angle

    // count points
    state score = 0
    equation score = {1 when (x <= 0 or x >= X_SIZE), 0 otherwise}
    equation reset = score or t == 0
    equation reset_d = reset[t[-1]]
    b.reset = reset or reset_d
    equation player1 = player1 + {score when x < X_SIZE/2, 0 otherwise}
    equation player2 = player2 + {score when x > X_SIZE/2, 0 otherwise}

    output reset with {condition=reset}
    output scores = (player1, player2)
    output angle_l = b.angle_v with {condition=impact_l}
    output angle_r = b.angle_v with {condition=impact_r}
    
    solver=forwardeuler{dt=0.01}
end
