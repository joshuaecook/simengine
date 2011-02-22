/*
 * Rossler Demo
 * 
 * Copyright 2010 Simatra Modeling Technologies
 */
model (x,y,z) = rossler(a,b,c)

    // define three scalar input parameters to the system
    input a with {default=0.2}
    input b with {default=0.2}
    input c with {default=5.7}
    
    // each of the three state variables have initial value equal to zero
    state x = 0
    state y = 0
    state z = 0

    // define each of the three differential equations
    equations
	x' = -y - z
	y' = x + a * y
	z' = b + z * (x - c)
    end

    // use the ode45 solver, our 4th order accurate, Dormand-Prince pair, variable time step solver
    solver = ode45 {reltol=1e-8, abstol=1e-8}

end
