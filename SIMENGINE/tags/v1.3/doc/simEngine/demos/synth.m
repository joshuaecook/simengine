clear input
input.w = 440
o = simex([simexamplepath '/demos/synth/lfo.dsl'], 0.01, input)
simplot(o)
