// Bouncing Ball - model of a ball bouncing in a box
// Copyright 2010 Simatra Modeling Technologies, L.L.C.
model (position) = bouncing_ball()

      // Define simulation parameters
      constant height = 10    // m
      constant velocity = 25  // m/s
      constant angle = pi/4   // radians
      constant damping = 0.8
      constant drag = 0.1
      constant wall = 10      // m
      constant ceiling = 15   // m
      constant gravity = -9.8 // m/s^2

      // Declare state initial values
      state x = 0
      state y = height // initial height
      state v_x = velocity*cos(angle)
      state v_y = velocity*sin(angle)

      // Equations describing the motion of the ball
      equations
	
	// compute the displacement as a function of velocity
	x' = v_x
	y' = v_y

        // compute the velocity taking into account ...
	v_x' = -drag*v_x          // drag
	v_y' = gravity - drag*v_y // and gravity

	// we want to set boundaries for our world that the ball bounces in
	// if the y position exceeds the ceiling, reduce the value
	y = (ceiling-(y-ceiling)) when y > ceiling
	y = -y when y < 0 // go back up after hitting the ground
	v_y = -v_y*damping when y < 0 or y > ceiling // invert the velocity

	// do the same for the walls at x=0 and x=wall
	x = (wall-(x-wall)) when x > wall
	x = -x when x < 0
	v_x = -v_x*damping when x > wall or x < 0

      end

      // output the position as an (x,y) pair
      output position = (x,y)

      // define the solver to be forwardEuler
      solver=forwardeuler{dt=0.01}
end