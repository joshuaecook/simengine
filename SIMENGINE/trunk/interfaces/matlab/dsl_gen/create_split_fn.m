function m = create_split_fn

% Create u function
m_u = Model('u_fun');

I = m_u.addInput('I', 2.0);
b0 = m_u.addInput('b0', 2.0);
b1 = m_u.addInput('b1', 1.5);
e = m_u.addInput('e', 0.1);
w = m_u.addInput('w');
u = m_u.addState('u', 1);

m_u.addDiffEq(u, u-u^3/3-w+I);
m_u.addOutput(u);

% Create w function
m_w = Model('w_fun');

I = m_w.addInput('I', 2.0);
b0 = m_w.addInput('b0', 2.0);
b1 = m_w.addInput('b1', 1.5);
e = m_w.addInput('e', 0.1);
u = m_w.addInput('u');
w = m_w.addState('w', 1);

m_w.addDiffEq(w, e*(b0 + b1*u - w));
m_w.addOutput(w);

% Instantiate one model
m = Model('fn');
I = m.addInput('I', 2.0);
b0 = m.addInput('b0', 2.0);
b1 = m.addInput('b1', 1.5);
e = m.addInput('e', 0.1);

inst1 = m.addInstance(m_u);
inst2 = m.addInstance(m_w);

% Assign inputs
instances = {inst1, inst2};
for i=1:length(instances)
    inst = instances{i};
    inst.I = I;
    inst.b0 = b0;
    inst.b1 = b1;
    inst.e = e;
end

% Create temporaries
u = m.addEq('u', inst1.u);
w = m.addEq('w', inst2.w);

% Assign states
inst2.u = u;
inst1.w = w;

% Assign outputs
m.addOutput(u);
m.addOutput(w);

end


