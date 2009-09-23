// function [out1,out2,out3] = vdpex(t,y)
// %VDPEX	Van der Pol equation in relaxation oscillation.
// %	The parameters here were specified by Shampine.  The limit cycle has
// %	portions where the solution components change slowly and the problem
// %	is quite stiff alternating with regions of very sharp change,
// %	quasi-discontinuities, where it is not stiff.  The initial
// %	conditions are close to a portion of slow change so as to test
// %	schemes for the selection of the initial step size.
// %	
// %	L. F. Shampine, Evaluation of a test set for stiff ODE solvers, ACM
// %	Trans. Math. Soft., 7 (1981) pp. 409-420.
// %	
// %	See also ODE15S, ODE23S, ODESET, VDPJAC

// %	Mark W. Reichelt and Lawrence F. Shampine, 3-23-94, 4-19-94
// %	Copyright (c) 1984-95 by The MathWorks, Inc.

// if length(t) == 0 			% return default tspan, y0 and options
//   out1 = [0; 3000];
//   out2 = [2; 0];
//   out3 = odeset('vectorized');
//   return;
// end

// dy = zeros(size(y)); 			% preallocate vector dy

// dy(1,:) = y(2,:);

// dy(2,:) = 1000*(1 - y(1,:).^2).*y(2,:) - y(1,:);

// out1 = dy;
model (y) = vdpex

  state y = 2
  state y1 = 0

  equations
    y' = y1
    y1' = 1000*(1-y^2)*y1-y
  end

end