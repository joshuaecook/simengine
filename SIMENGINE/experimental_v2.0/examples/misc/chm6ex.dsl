// function [out1,out2,out3] = chm6ex(t,y)
// %CHM6EX	Problem CHM6 from Enright and Hull.
// %	This four equation system models catalytic fluidized bed dynamics.
// %	A small absolute error tolerance is necessary because y(:,2) ranges
// %	from 7e-10 down to 1e-12.  A suitable 'atol' is 1e-13 for all
// %	solution components.  With this choice the solution curves computed
// %	with ODE15S are plausible.  Because the step sizes span 15 orders of
// %	magnitude, a loglog plot is appropriate.
// %	
// %	W. H. Enright and T. E. Hull, Comparing Numerical Methods for the
// %	Solution of Stiff Systems of ODEs Arising in Chemistry, in Numerical
// %	Methods for Differential Systems, L. Lapidus and W. E. Schiesser
// %	eds., Academic Press, Orlando, FL, 1976, pp 45-67.
// %	
// %	See also ODE15S, ODE23S, ODESET, CHM6JAC

// %	Mark W. Reichelt and Lawrence F. Shampine, 6-20-94
// %	Copyright (c) 1984-95 by The MathWorks, Inc.

// if length(t) == 0 			% return default tspan, y0 and options
//   out1 = [0; 1000];
//   out2 = [761; 0; 600; 0.1];
//   out3 = odeset('atol',1e-13,'vectorized');
//   return;
// end

// dy = zeros(size(y)); 			% preallocate vector dy

// K = exp(20.7 - 1500 ./ y(1,:));

// dy(1,:) = 1.3*(y(3,:) - y(1,:)) + 10400 * K .* y(2,:);

// dy(2,:) = 1880 * (y(4,:) - y(2,:) .* (1+K));

// dy(3,:) = 1752 - 269*y(3,:) + 267*y(1,:);

// dy(4,:) = 0.1 + 320*y(2,:) - 321*y(4,:);

// out1 = dy;
model (y) = chm6ex

  state x = 761
  state y = 0
  state z = 600
  state w = 0.1

  equations
    K = exp(20.7 - 1500 / x)

    x' = 1.3*(z - x) + 10400 * K * y

    y' = 1880 * (w - y * (1+K))
 
    z' = 1752 - 269*z + 267*x

    w' = 0.1 + 320*y - 321*w
  end

end