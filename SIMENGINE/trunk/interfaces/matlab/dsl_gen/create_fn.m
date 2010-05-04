function m = create_fn

m = Model('fn');

I = m.addInput('I', 2.0);
b0 = m.addInput('b0', 2.0);
b1 = m.addInput('b1', 1.5);
e = m.addInput('e', 0.1);

u = m.addState('u', 1);
w = m.addState('w', 1);

m.addDiffEq(u, u-u^3/3-w+I);
m.addDiffEq(w, e*(b0 + b1*u - w));

m.addOutput(u);
m.addOutput(w);

end

