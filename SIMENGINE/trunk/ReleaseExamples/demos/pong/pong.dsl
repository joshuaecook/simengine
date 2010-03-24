/*
 * PONG simulator using simEngine
 *
 * Copyright 2010 Simatra Modeling Technologies
 */

// definitions
constant SCREEN_WIDTH = 100
constant SCREEN_HEIGHT = 30
constant PADDLE_HEIGHT = 6
constant HALF_PADDLE = 3
constant BALL_SPEED = 7
constant PADDLE_SPEED = 10

/*
 * pong_paddle - simulates the motion of the pong paddle up and down on the screen
 *
 * inputs: 
 *   ball_x, ball_y - the paddle can see the ball so it can hone in on its position
 *   approaching - the approaching flag alerts the paddle if it needs to track the ball
 * 
 * output:
 *   pos - the position along the y axis of the paddle
 *
 */
model (pos) = pong_paddle(ball_x, ball_y, approaching)
    
    // define two states in the paddle, a state for the position and a state to store its velocity
    state pos = SCREEN_HEIGHT/2
    state v = 0

    // define the change in position over time as the velocity
    equation pos' = v
    equation v' = 0

    // define "update" equations that will adjust the state variables after they are computed if a condition is set
    equations
	// the paddle will move only when the ball is approaching
	v = 0 when not(approaching)
	v = (ball_y - pos) when approaching

	// Make sure the paddle speed doesn't go too fast
	v = PADDLE_SPEED when v > PADDLE_SPEED
	v = -PADDLE_SPEED when v < -PADDLE_SPEED

	// stop the paddle if it hits the boundaries
	v = 0 when pos + HALF_PADDLE >= SCREEN_HEIGHT
	v = 0 when pos - HALF_PADDLE <= 0

	// set maximums for the paddle so it does not go off the screen
	pos = HALF_PADDLE when pos < HALF_PADDLE // bottom of the screen
	pos = SCREEN_HEIGHT-HALF_PADDLE when pos > SCREEN_HEIGHT-HALF_PADDLE // top of the screen
    end
end

/*
 * pong_ball - simulates the motion of the pong ball
 *
 * inputs: 
 *   impact_l, impact_r - flags to tell the ball that it impacted the left or right paddles, respectively
 *   reset - flag to reset the ball to the center position after a point is scored
 *   divert_angle - angle adjustment is computed following an impact based on where the ball hits the paddle
 * 
 * output:
 *   x, y - the (x,y) coordinates of the pong ball
 *
 */
model (x, y, v_x, v_y) = pong_ball(impact_l, impact_r, reset, divert_angle)

    // define the states of the system - the position is a second derivative equation
    // which is broken down into two first order differential equations and further separated
    // for the x direction and y direction.
    state x = SCREEN_WIDTH/2    // position
    state y = SCREEN_HEIGHT/2
    state v_x = 0               // velocity
    state v_y = 0

        
    // define each of the differential equations
    equations
	// define the change in position as velocity
	x' = v_x
	y' = v_y
	
	// set the velocity static if nothing happens, but "update" it if there's an impact or a reset
	v_x' = 0
	v_y' = 0
    end

    // Reflect the ball off the paddle. Reflect only if an impact occurs, which is passed in as an input.
    // Depending on where the ball hits the paddle, a divert angle will be calculated and passed in as a
    // parameter.
    equations
	// Convert the velocity from rectangular coordinates to polar coordinates
	mag_v = sqrt(v_x^2 + v_y^2)
	angle_v = atan2(v_y,v_x)

	// Convert the velocity back to rectangular coordinates and update the value
	// adding in the divert angle.  Only update the velocity in this block
	// if there's an impact
	combined = angle_v+divert_angle
	v_x = -mag_v * cos(combined) when impact_l or impact_r
	v_y = mag_v * sin(combined) when impact_l or impact_r
    end
    
    // stop the ball when the ball goes off the screen on the left/right
    equation v_x = 0 when x < 0 or x > SCREEN_WIDTH
    equation v_y = 0 when x < 0 or x > SCREEN_WIDTH

    // reflect the ball off the top and bottom walls
    equation v_y = -v_y when y <= 0 or y >= SCREEN_HEIGHT

    // the reset input will be set when a point is scored or when the game is started
    // move the ball to the center and start it's motion with a random velocity.  We first
    // define random variables to control the initial direction and speed following a point
    random mag with {normal, mean=BALL_SPEED, stddev=BALL_SPEED/5}
    random ang with {uniform, low=0, high=pi}

    // limit the angle from 45 deg to -45 deg and from 135 deg to 225 deg
    equation angle = {ang        when ang <= pi/4, 
		      ang + pi/2 when ang <= pi/2,
		      ang + pi/2 when ang <= 3*pi/4,
		      ang + pi   otherwise}

    // now we update the position and velocity following the reset input
    equations
	// set the velocity to the randomly generated polar velocity
	v_x = mag*cos(angle) when reset
	v_y = mag*sin(angle) when reset

	// set the starting point to the center of the screen
	x = SCREEN_WIDTH/2 when reset
	y = SCREEN_HEIGHT/2 when reset
    end
end

/*
 * pong - simulates the game of pong
 *
 * output:
 *   paddle1, paddle2 - the position along the y axis of the paddle
 *   ball - the position of the ball
 *   scores - the running score of the game
 */
model (paddle1, paddle2, ball, scores) = pong()

    constant X_P1 = 3
    constant X_P2 = SCREEN_WIDTH - 3

    // define scores
    state player1 = 0
    state player2 = 0

    // define the ball first
    submodel pong_ball b
    output ball = (b.x, b.y, b.v_x, b.v_y)
    equation x = b.x
    equation y = b.y
    equation dx = x-x[t[-1]]

    // define a paddle
    submodel pong_paddle p1 with {ball_x = x, ball_y = y, approaching= dx < 0}
    submodel pong_paddle p2 with {ball_x = x, ball_y = y, approaching= dx > 0}
    output paddle1 = p1.pos
    output paddle2 = p2.pos    

    // determine impacts and divert angle
    equations
      // an impact is when the ball touches a paddle.  This impact will cause a velocity reversal and an angle diversion
      impact_l = dx < 0 and b.x < (X_P1+1) and b.x > X_P1 and p1.pos+HALF_PADDLE >= b.y and p1.pos-HALF_PADDLE <= b.y
      impact_r = dx > 0 and b.x > (X_P2-1) and b.x < X_P2 and p2.pos+HALF_PADDLE >= b.y and p2.pos-HALF_PADDLE <= b.y
      // based on where the ball hits the paddle, compute an angle offset
      divert_angle = {-(b.y - p1.pos)*pi/16 when impact_l,
		      (b.y - p2.pos)*pi/16 when impact_r,
		      0 otherwise}
    end    

    // verify that only one impact is recorded
    b.impact_l = impact_l and not(impact_l[t[-1]])
    b.impact_r = impact_r and not(impact_r[t[-1]])
    b.divert_angle = divert_angle

    // count points
    state score = 0
    equation score = {1 when (x <= 0 or x >= SCREEN_WIDTH), 0 otherwise}

    // produce resets that will start the ball in the center of the screen
    equation reset = score or t == 0
    b.reset = reset[t[-1]]

    // tally up the players scores
    equation player1 = player1 + {score when x > SCREEN_WIDTH/2, 0 otherwise}
    equation player2 = player2 + {score when x < SCREEN_WIDTH/2, 0 otherwise}

    // define a score quantity that includes both players scores in one output at each time point
    output scores = (player1, player2)
    
    // set the solver to forward euler
    solver=forwardeuler{dt=0.01}
end
