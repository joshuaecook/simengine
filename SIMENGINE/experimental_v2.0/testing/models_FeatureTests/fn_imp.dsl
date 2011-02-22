/*
 *   FitzHugh-Nagumo model of a simplified Hodgkin-Huxley neuron model
 *   Derived from FitzHugh R. (1955, 1961) and Nagumo J., et al. (1962)
 *   Copyright 2007-2009 Simatra Modeling Technolgies
 */
model (u_w) = fn_imp(b0, b1, e, I)

    input b0 with {default=2}
    input b1 with {default=1.5}
    input e with {default=0.1}
    input I with {default=2}
    
    iterator t_exp with {continuous, solver=forwardeuler{dt=0.1}}
    iterator t_imp with {continuous, solver=linearbackwardeuler{dt=0.1}}

    state u = 0 with {iter=t_exp}
    state w = 0 with {iter=t_imp}

    equations
	u' = u - u*u*u / 3 - w + I
	w' = e * (b0 + b1 * u - w)
    end

    output u_w[t_imp] = (u,w)

end
