//
// Izhikevich Quadratic Integrate and Fire
// Izhikevich, E.M., Simple Model of Spiking Neurons, IEEE Trans on Neural Networks, Nov 2003
// Adapted for use with simEngine
// 
// Copyright 2009 Simatra Modeling Technologies
//
model (v) = QuadIandF(a, b, c, d, I)

input a with {default=0.02}
input b with {default=0.2}
input c with {default=-65}
input d with {default=2}

state v = -65 // membrane potential
state u = 0 // membrane recovery variable

equations
    // dynamic equations
    v' = 0.04*v^2 + 5*v + 140 - u + I
    u' = a*(b*v-u)

    // reset equations
    threshold = v >= 30
    v = c when threshold
    u = u + d when threshold
end

end
