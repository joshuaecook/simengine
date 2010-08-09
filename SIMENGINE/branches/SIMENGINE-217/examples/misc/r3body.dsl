// function [out1,out2,out3] = r3body(t,y)
// %R3BODY	A restricted 3 body problem with periodic solution.
// %	This is a standard test problem for non-stiff solvers stated in
// %	Shampine and Gordon, p. 246 ff.  The first two solution components
// %	are coordinates of the body of infinitesimal mass, so plotting one
// %	against the other gives the orbit of the body around the other two
// %	bodies.  The initial conditions have been chosen so as to make the
// %	orbit periodic.  Moderately stringent tolerances are necessary to
// %	reproduce the qualitative behavior of the orbit.  Suitable values
// %	are 1e-5 for 'rtol' and and 1e-4 for 'atol'.
// %	
// %	L. F. Shampine and M. K. Gordon, Computer Solution of Ordinary
// %	Differential Equations, W.H. Freeman & Co., 1975.
// %	
// %	See also ODE45, ODE23, ODE113, ODESET

// %	Mark W. Reichelt and Lawrence F. Shampine, 3-23-94, 4-19-94
// %	Copyright (c) 1984-95 by The MathWorks, Inc.

// if length(t) == 0 			% return default tspan, y0 and options
//   out1 = [0; 6.19216933131963970674];
//   out2 = [1.2; 0; 0; -1.04935750983031990726];
//   out3 = odeset('rtol',1e-5,'atol',1e-4);
//   return;
// end

// dy = zeros(size(y)); 			% preallocate vector dy
// mu = 1 / 82.45;
// mustar = 1 - mu;
// r13 = ((y(1) + mu)^2 + y(2)^2) ^ 1.5;
// r23 = ((y(1) - mustar)^2 + y(2)^2) ^ 1.5;

// dy(1) = y(3);

// dy(2) = y(4);

// dy(3) = 2*y(4) + y(1) - mustar*((y(1) + mu)/r13) - mu*((y(1) - mustar)/r23);

// dy(4) = -2*y(3) + y(2) - mustar*(y(2)/r13) - mu*(y(2)/r23);

// out1 = dy;
model (y1, y2, y3, y4) = r3body

  state y1 = 1.2
  state y2 = 0
  state y3 = 0
  state y4 = -1.04935750983031990726

  equations
    mu = 1 / 82.45
    mustar = 1 - mu
    r13 = ((y1 + mu)^2 + y2^2) ^ 1.5
    r23 = ((y1 - mustar)^2 + y2^2) ^ 1.5

    y1' = y3

    y2' = y4

    y3' = 2*y4 + y1 - mustar*((y1 + mu)/r13) - mu*((y1 - mustar)/r23)

    y4' = -2*y3 + y2 - mustar*(y2/r13) - mu*(y2/r23)
  end

//  solver=rk4(0.001) // WORKS!!!
//    solver=forwardeuler(0.00001) // SORT OF WORKS, not really...
  solver=ode23//(0.01, 10e-5, 10e-4)
    // plot y1 vs. y2

end