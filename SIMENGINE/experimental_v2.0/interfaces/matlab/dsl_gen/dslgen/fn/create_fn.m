function m = create_fn

m = Model('fn');

I = m.input('I', 2.0);
b0 = m.input('b0', 2.0);
b1 = m.input('b1', 1.5);
e = m.input('e', 0.1);

u = m.state(1);
w = m.state(1);

m.diffequ(u, u-u^3/3-w+I);
m.diffequ(w, e*(b0 + b1*u - w));

m.output('u', u);
m.output('w', w);


end

