model (sine)=sine_model
  state x = 0
  state y = 0

  output sine = (x,y)

  equations
    x' = 1 - y
    y' = x - 1
  end

  solver = linearbackwardeuler{dt=0.01}

end

/*
 	STATE x (-0.1 TO 2.1 BY 0.00001) = 0;
	STATE y (-0.1 TO 2.1 BY 0.00001) = 1;
	STATE z (-0.1 TO 2.1 BY 0.00001) = 2;
	STATE t (0 TO 100 BY 0.001) = 0;

	CONSTANT dt = 1e-1;
	FUN integrate (dt, t, state, eq) = state + dt * eq(t);
	OUTPUT "x","y","z","t";
	t = t + dt;

	d(x) = 1 - y;
	d(y) = x-1;

*/
