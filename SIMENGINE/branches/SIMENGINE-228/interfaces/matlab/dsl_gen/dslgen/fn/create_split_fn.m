function m = create_split_fn

% Define an iterator
t_imp = Iterator('t_imp', 'continuous', 'solver', 'linearbackwardeuler', 'dt', 1);
t_exp = Iterator('t_exp', 'continuous', 'solver', 'forwardeuler', 'dt', 0.1);

% Create u function
m_u = Model('u_fun');

I = m_u.input('I', 2.0);
b0 = m_u.input('b0', 2.0);
b1 = m_u.input('b1', 1.5);
e = m_u.input('e', 0.1);
w = m_u.input('w');
u = m_u.state(1, 'iter', t_exp);

m_u.diffequ(u, u-u^3/3-w+I);
m_u.output('u', u);

% Create w function
m_w = Model('w_fun');

I = m_w.input('I', 2.0);
b0 = m_w.input('b0', 2.0);
b1 = m_w.input('b1', 1.5);
e = m_w.input('e', 0.1);
u = m_w.input('u');
w = m_w.state(1, 'iter', t_imp);

m_w.diffequ(w, e*(b0 + b1*u - w));
m_w.output('w', w);

% Instantiate one model
m = Model('fn');
I = m.input('I', 2.0);
b0 = m.input('b0', 2.0);
b1 = m.input('b1', 1.5);
e = m.input('e', 0.1);

inst1 = m.submodel(m_u);
inst2 = m.submodel(m_w);

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
u = m.equ('u', inst1.u);
w = m.equ('w', inst2.w);

% Assign states
inst2.u = u;
inst1.w = w;

% Assign outputs
m.output('fn', u, w, 'iter', t_imp);

end


