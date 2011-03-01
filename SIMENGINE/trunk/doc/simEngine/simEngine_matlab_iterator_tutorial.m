%% simEngine Iterator Tutorial
%
% Iterators are a powerful feature in simEngine.  Iterators allow you to
% create models that run at one or more time steps, partition state variables across
% multiple solvers, mix recurrence equations with differential equations,
% and read from streaming inputs.  This following tutorial will show you
% step by step how to define iterators and how to exploit them for your
% modeling problems.
%

%% Using an iterator to define time
%
% In the previous tutorial examples, there was an implicit universal time variable _t_ for the
% model: time was always incrementally increasing
% through the simulation of the model according to the time step of the
% solver.  simEngine provides
% the capability to define multiple "time" quantities simultaneously in a
% model.  These "time" quantities are referred to as iterators.
%
% When we used the commands

mdl.solver = 'forwardeuler';
mdl.dt = 0.001;

%%
% we were implicitly updating the default iterator, _t_ of the model.  For more
% fine grained control of time, we can explicitly define our own iterators.
%  These iterators can be of two types, a continuous iterator representing
%  time as a smooth, monotonically increasing quantity, or a discrete
%  iterator which increases in fixed step sizes.  As we will see below,
%  multiple iterators of each type can be used simultaneously within the same
%  model.

%%
% As a first example, we'll use the exponential decay model with
% an explicit iterator.  First, we define the iterator by calling the
% *Iterator* class constructor.  We're going to set up this time iterator
% to use the rk4 (4th order Runge-Kutta method) solver.

t = Iterator('continuous', 'solver', 'rk4', 'dt', 0.01);

%%
% Now, to use it as part of a model, we can either define inputs, outputs,
% or states to use this iterator explicitly, or we can define it as the
% default iterator for this model.  We can start now with the latter
% method and continue on with the definition of the model.

mdl = Model('ExponentialDecay', t);

k = mdl.input('k', 1);
x = mdl.state(20); % by default, this will use the 't' iterator
mdl.diffequ(x, -k*x);

%%
% In this version, let's add an additional sine wave that will be
% attenuated by the decay function.  To do this, we need to pull out the
% value of _t_.  We can't necessarily use _t_ in a computation directly
% because it's not a simEngine expression, but rather an Iterator.  Instead,
% we need to convert it to an expression type by using the *Exp* object
% constructor.

time = Exp(t);

%%
% We can also do this by accessing the time method
% in the *Model* object.

time = mdl.time;

%%
% Now, we can create a sine wave based on time.

f = 3;
y = sin(2*pi*f*time);

%%
% For the output, we can create a new variable z defined as the attenuation
% of the sine wave.

mdl.output('z', x*y);

%%
% Visualizing the results shows us the attenuated sine wave as expected.

data = mdl.simex(3);
simplot(data);

%% Downsampling with iterators
% Downsampling is one of the most common uses for adding a discrete
% iterator into a continuous time system.  For this example, let's create a
% simple differential equation representation for a sine wave.
%
% We'll first define a time iterator and use the heun method, a
% particularly good method for periodic output.

t = Iterator('continuous', 'solver', 'heun', 'dt', 0.01);

%%
% Now, we'll define the sine wave model and apply this _t_ iterator to each
% of the states.

% create the Model object
mdl = Model('sine');

% define the states with the 'iter' property to explicitly set the iterator
% for the state
x = mdl.state(0, 'iter', t);
y = mdl.state(1, 'iter', t);

% define the differential equations describing the evolution of these two
% states to produce a sinusoidal output
mdl.diffequ(x, y-1);
mdl.diffequ(y, 1-x);

%%
% We can now create multiple outputs using differing amounts of
% downsampling.  We first need to define the time points at which an output
% is going to be read.  This is done with a discrete time iterator.  We'll
% define two for illustration.  The 'sample_period' property of a discrete
% iterator corresponds to the 'dt' property of a continuous iterator.  If
% the continuous iterator, _t_, is evaluated 100 times per second, then
% the n10 and n100 iterators are evaluated 10 times and once per second,
% respectively.

n10 = Iterator('discrete', 'sample_period', 0.1);
n100 = Iterator('discrete', 'sample_period', 1);

%%
% Three outputs will be created, one based on the original _t_ iterator,
% and the others based on the two generated discrete iterators, _n10_ and
% _n100_.

mdl.output('x1', x);
mdl.output('x2', x, 'iter', n10);
mdl.output('x3', x, 'iter', n100);

%%
% The output _x1_ does not need an iterator explicitly defined.  This is
% because the state _x_ has already been defined with an iterator, and
% simEngine is smart enough to propagate the iterators through the
% equations to the outputs.
%
% We can now simulate the model and see what happens.

data = mdl.simex(10);
simplot(data.x1, '-', data.x2, '-^', data.x3, '-o');
legend('x1', 'x2', 'x3');

%%
% We can see that the x2 trace is almost identical to the x1 trace
% suggesting that the downsampling had little effect on the data.  For x3,
% however, we can see that downsampling lost significantly more fidelity,
% but with only 1% of the data being saved, the sine wave was still clearly
% recognizable.

%% Mixing algebraic and differential equations with one iterator
% It is a common problem to want to perform some type of analysis of data
% generated from computing a set of differential equations.  There are
% generally two options to perform this analysis:
%
% * Perform this analysis after the simulation completes
% * Perform this analysis during the simulation
%
% The latter option is generally preferred for two reasons.  First, the
% analysis can be computationally intensive, so performing as part of a
% simEngine simulation can yield better performance.  And second, analysis
% of data often reduces the amount of data returned from a simulation,
% which can improve overall performance.
%

% Define the time iterator - explicitly giving it the name 't'
t = Iterator('t', 'continuous', 'solver', 'ode23');

% Create a new model called 'neuron'
mdl = Model('neuron', t);

% Define an external current input with default 0
Iext = mdl.input('Iext', 0);

% Set some constant values
Cm = 1;
gNa = 120;
gK = 100;
gleak = 0.51;
ENa = 55;
EK = -80;
Eleak = -55;

% Define three state variables with initial values
Vm = mdl.state(-45);
hNa = mdl.state(0.9);
mK = mdl.state(0.1);

% Create two useful functions
xinf = @(a, b)(1/(1+exp((Vm + a)/b)));
tau = @(a, b, c, d)(a/(exp((Vm+b)/c) + exp(-(Vm+b)/d)));

% List all the equations of the model
INa = gNa*xinf(35, -7.8)^3*hNa*(Vm - ENa);
IK = gK*mK^4*(Vm - EK);
Ileak = gleak*(Vm - Eleak);

% Define differential equations for the state variables
mdl.diffequ(hNa, (xinf(55, 7) - hNa)/tau(30, 50, 15, 16));
mdl.diffequ(mK, (xinf(28, -15) - mK)/tau(7, 40, 40, 50));
mdl.diffequ(Vm, -(1/Cm)*(INa + IK + Ileak - Iext));

% Return the output voltage potential
mdl.output(Vm);

%%
% Let's run the model to see what it does.  This time, we're running the
% neuron model with a variable time step solver, so the number of samples
% returned will be dramatically less than if we used a fixed time-step
% solver, such as _rk4_ or _forwardeuler_.

data = mdl.simex(100);
simplot(data, '-o');

%%
% To analyze this data, let's say that all we want are the times when a
% spike occurs and the total number of spikes.  We can filter our data through a simple recognition
% algorithm where we are looking at three points at a time, where the
% middle point is greater than the earlier point and the later point.
%
% First, to determine if a spike occurs, we have to add new syntax.
% Effectively, we would like to find the value of Vm at previous points in
% time.  simEngine provides a way to do this through iterator indexing.

previous_Vm = Vm(t-1);
previous_previous_Vm = Vm(t-2);

%%
% Using relative indexing of iterators, simEngine knows that you are asking
% for two samples back in time.  Multiple calls to previous values can be
% included on one line, creating a very simple expression to determine if a
% spike occurs.

spike_occurred = Vm(t) < Vm(t-1) & Vm(t-1) >= Vm(t-2);

%%
% Now that we know when a spike occurred, we can simply return just the
% times that it happened.  In the statement below, we're using a special
% syntax that tells simEngine to output only the time when a spike
% occurred.  

mdl.output(t, 'when', spike_occurred);

% now run the simulation and verify the spike times
data = mdl.simex(100);
data.t

%%
% Finally, let's create another state to count the number of spikes.  Set
% the initial value to zero.

spike_count = mdl.state(0);

%%
% When we define a state for a differential equation, we first define the
% state, and then apply a differential equation to that state.  In this
% case, we do not want to add a differential equation, but instead a simple
% algebraic expression.  In fact, we're going to add a recurrence equation
% to spike_count.

mdl.recurrenceequ(spike_count, piecewise(spike_count+1, spike_occurred, ...
                                         spike_count));


%%
% In the above equation, we're saying that spike count at time $t_{+1}$ is
% equal to spike count plus 1 whenever a spike occurred, but otherwise,
% retain the current value of spike count.
%
% Running the simulation, we can see by inspecting the last value of
% finalStates, that are total number of spikes has been properly recorded
% as 8.

[data, finalStates, finalTime] = mdl.simex(100);
spike_count = finalStates(end)


%% Mixing two solvers in one model
% In simEngine, we've included many different integration methods because,
% depending on your problem, some may provide the accuracy that you
% require, or the stability characteristics, or the performance you need.
% In some models, you might find that a one size fits all solution is not
% particularly ideal, that some states have different accuracy, stability,
% or performance requirements than others.
%
% This is why we introduced the ability to specify multiple solvers in
% simEngine through the use of multiple iterators.  For example, you might
% choose to have a fast changing state unpdate with a smaller timestep than
% a slow changing state.  Or, there might be some states that have their
% stiffness tied to their linear term, where we have the exponential Euler
% method. In another case, we might be solving the heat equation PDE
% (partial differential equation) where stiffness is guaranteed, along with
% other equations where stability is not an issue, but performance is.
%
% As a simple example, let's solve our original basic neuron model, but
% let's use a different solver, exponential Euler, that is often used for computing the evolution of
% gating variables in computational neuroscience.  Even if we choose to use exponential Euler for the
% gating variables, we can still use forward Euler for the voltage
% potential, just as we did before.
%
% When using multi-iterators, we can set one iterator to be
% the default iterator and explicitly define the others.  To make the model
% more clear to read, it often makes sense to be explicit with every
% iterator.  That is how we're going to proceed in this example.
%
% We start out by defining two iterators:

dt = 0.001; % define a common timestep, though it doesn't have to be the same
t_fwdeuler = Iterator('continuous', 'solver', 'forwardeuler', 'dt', dt);
t_expeuler = Iterator('continuous', 'solver', 'exponentialeuler', 'dt', dt);

%%
% Now, we can continue by defining the model as we did before, but this
% time, we can be explicit when we define the states as to which iterator,
% and therefore, which solver to use.

% Create a new model called 'neuron'
mdl = Model('neuron');

% Define an external current input with default 0
Iext = mdl.input('Iext', 0);

% Set some constant values
Cm = 1;
gNa = 120;
gK = 100;
gleak = 0.51;
ENa = 55;
EK = -80;
Eleak = -55;

%%
% Here, we're going to define our states with explicit iterators.  This is
% done by adding the 'iter' keyword followed by the Iterator.

% Define three state variables with initial values
Vm = mdl.state(-45, 'iter', t_fwdeuler);
hNa = mdl.state(0.9, 'iter', t_expeuler);
mK = mdl.state(0.1, 'iter', t_expeuler);

%%
% List all the equations of the model
mNaInf = 1/(1+exp((Vm+35)/-7.8));
hNaInf = 1/(1+exp((Vm+55)/7));
mKInf = 1/(1+exp((Vm+28)/-15));
hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16));
mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50));
INa = gNa*mNaInf^3*hNa*(Vm - ENa);
IK = gK*mK^4*(Vm - EK);
Ileak = gleak*(Vm - Eleak);

% Define differential equations for the state variables
mdl.diffequ(hNa, (hNaInf - hNa)/hNaTau);
mdl.diffequ(mK, (mKInf - mK)/mKTau);
mdl.diffequ(Vm, -(1/Cm)*(INa + IK + Ileak - Iext));

% Return the output voltage potential
mdl.output(Vm);

%%
% Now that the model is created, we can simulate it and plot the
% results.

data = mdl.simex(100);
simplot(data);

%%
% Much more complicated models can be generated as desired.  For example,
% two iterators can be at different time steps such that a portion of a
% model runs at a different rate than another part of the model.  This can
% improve performance of large models.  When different iterators with
% differing time steps are used, please note that there is no interpolation
% performed when reading data from one iterator to another.  This means
% that if one state is being updated based on the value of a state being
% generated from a different iterator, then it will use the last generated
% value instead of an interpolated value represent the exact time required.
% For this reason, we recommend that iterators are always defined to be
% multiples of each other, and that fixed time step iterators are not mixed with
% variable time step iterators.
%
% Additionally, please note that multiple iterator support and the use of
% the exponential Euler method are limited to simEngine Standard and
% simEngine Professional editions.

%% Multiple iterators for one output
%
% A common error that simEngine returns occurs when you outputting a
% quantity that depends on data produced from two different iterators.  If
% there are two iterators, and they're at different time steps, how often
% should simEngine write the results?
%
% See the following example:

% Define a Model object
mdl = Model('TwoSineWaves');

% Define two different iterators
n_1k = Iterator('n_1k', 'discrete', 'sample_frequency', 1e3);
n_10k = Iterator('n_10k', 'discrete', 'sample_frequency', 1e4);

% Create two sine waves
y_1k = sin(2*pi*50*Exp(n_1k));
y_10k = sin(2*pi*500*Exp(n_10k));

% Output each of the sine waves
mdl.output(y_1k);
mdl.output(y_10k);

% Simulate and plot the results
data = mdl.simex(0.02, '-debug');
simplot(data.y_1k, '-+', data.y_10k, '-x');
legend('y_{1k}', 'y_{10k}');

%%
% So the above worked fine and you can see by the points that each of the
% sine waves was generated at a different sampling rate.  However, what
% happens if we try to add another output that is a function of both sine
% waves, for example the product of both waves.

% Create the product of the sine waves
mdl.output('prod', y_1k*y_10k);
% Simulate and plot the results
try
    data = mdl.simex(0.02);
    simplot(data.prod);
catch
end

%%
% simEngine returned an error saying that it doesn't know what to do - you
% have two iterators driving one output.  How many samples should the
% output contain.  In this case, we want the output sampled at the higher
% rate, otherwise we're going to lose fidelity.
%
% We do this by adding an explicit iterator to the output.

mdl.output('prod', y_1k*y_10k, 'iter', n_10k);
data = mdl.simex(0.02);
simplot(data.prod);

%%
% Now we have the properly modulated output that we are expecting. Please
% keep in mind that when we upsample a quantity in simEngine, we do not
% perform any interpolation.  You can see the ramifications of that if we
% create a new output that shows the slower sine wave at the faster
% iterator.
%

mdl.output('y_1k_resampled', y_1k, 'iter', n_10k);
data = mdl.simex(0.02);
simplot(data.y_1k, '-+', data.y_1k_resampled, '-x');

%% Using iterators for inputs
%
% simEngine gives you lots of flexibility when it comes to defining inputs
% of your model.  Inputs can be scalar parameters or they can streaming
% inputs.  They can be used as a start and stop signal to your model, or
% they can be configured to repeat a sequence of input over and over again.
%
% In this example, let's perform a neural simulation where the input is
% defined as an array from the Matlab workspace.  The Neuron model is
% typically executed with either current steps or a current ramp.
%
% To begin, we need to determine what the sampling rate of our input will
% be.  All input data that is not passed in as a scalar value must have a
% set sampling rate.  Let's choose 1/10 of the rate of the integrator, just
% to reduce the data size.  For the neuron model, the time step (dt) for
% the solver was set to 0.01 ms.  We can use the 0.1 ms as a sample period.

Ts = 0.1; % sample period

%%
% The step can be off for 20 ms, reach it's amplitude for the next 60 ms,
% and then stay off for the remaining 20 ms of a 100 ms simulation.

stop_time = 100;
t = 0:Ts:stop_time;
amplitude = 10;
Istep = zeros(1, length(t));
Istep(find(t >= 20 & t < 80)) = amplitude;
             
%%
% Just to verify, we can plot Iext to make sure it looks correct.

plot(t, Istep);
axis([0 stop_time -10 amplitude+10]);

%%
% Next, let's recreate our neuron model. We'll make slight changes to it so
% it does not tonically fire - it won't create any spikes in the absence of
% external current input.

% Create a new model called 'neuron'
mdl = Model('neuron');
mdl.solver = 'forwardeuler';
mdl.dt = 0.01;

%%
% We have to do something different when defining the input.  That is, we
% have to tell it what the sampling rate of the input is.  We do that by
% creating an iterator with that sampling rate and then assigning the input
% to that iterator.

% Create an iterator with a sampling period of Ts.
n_input = Iterator('discrete', 'sample_period', Ts);

% Define an external current input with default 0 and assign it to Iterator
% n_input
Iext = mdl.input('Iext', 0, 'iter', n_input);

%%

% Set some constant values
Cm = 1;
gNa = 120;
gK = 100;
gleak = 0.51;
ENa = 55;
EK = -80;
Eleak = -60;

% Define three state variables with initial values
Vm = mdl.state(-60);
hNa = mdl.state(0.9);
mK = mdl.state(0.1);

% List all the equations of the model
mNaInf = 1/(1+exp((Vm+35)/-7.8));
hNaInf = 1/(1+exp((Vm+55)/7));
mKInf = 1/(1+exp((Vm+28)/-15));
hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16));
mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50));
INa = gNa*mNaInf^3*hNa*(Vm - ENa);
IK = gK*mK^4*(Vm - EK);
Ileak = gleak*(Vm - Eleak);

% Define differential equations for the state variables
mdl.diffequ(hNa, (hNaInf - hNa)/hNaTau);
mdl.diffequ(mK, (mKInf - mK)/mKTau);
mdl.diffequ(Vm, -(1/Cm)*(INa + IK + Ileak - Iext));

% Return the output voltage potential
mdl.output(Vm);

%% 
% After defining the current step and creating the model, we're ready to
% start simulating.  We begin by creating an input structure that includes
% the current step data followed by a call to *simex* and *simplot*.

input = struct('Iext', Istep);

% for illustration, we can create an output of the input
mdl.output(Iext);

data = mdl.simex(100, input);
simplot(data);

%%
% This example shows how we can create a step current and use it as an
% input stimulus to a simEngine model.  We can also create a set of step
% currents and pass in that set of currents to run parallel simulations.

amplitude = 0:5:20;
indices = find(t >= 20 & t < 80);
Istep = cell(1,length(amplitude));
for i=1:length(amplitude)
    Istep{i} = zeros(1, length(t));
    Istep{i}(indices) = amplitude(i);
end

%%
% Again, we'll verify just to make sure that the input steps look correct

simplot([t' reshape(cell2mat(Istep),1001,5)]);
axis([0 stop_time -10 max(amplitude)+10]);

%%
% Now, we run the simulation in the same way, but pass in the cell array of
% Isteps.  This by default takes advantage of your multi-core processor or
% multi-processor system, assuming you are running simEngine Professional
% or a Professional Trial.

input.Iext = Istep;
data = mdl.simex(100, input);

% Plot all the traces on top of each other
for i=1:length(data)
    subplot(length(data),1, i);
    simplot(data(i).Vm);
    title(['Amplitude = ' num2str(amplitude(i))]);
end

%% Scoping of iterators - global vs. local
%
% When you are creating a large model that is based on multiple models
% with multiple iterators, it is important that iterators
% are shared or duplicated to the greatest extent possible.  As an example, for a
% variable time step solver running across one model, you might want to
% make sure that every state contributes to choosing the timestep for the
% system, rather than having multiple variable step solvers with different time steps.  
% In another example, a large number of distributed models
% might each require their own independent solver.  simEngine provides a
% means to create global iterators or maintain local iterators per state or
% per model.
%
% First, we'll create a model that we'll use as a container to create
% examples.

mdl = Model('Scoping_of_Iterators');

%%
% Now, let's show an example of an iterator in the system and break down
% exactly what is going on.

% define a continuous iterator
t1 = Iterator('continuous', 'solver', 'ode45')

%%
% When we leave off the semicolon, we see that the iterator is described as a
% continuous iterator with a long complex name.  This name is randomly
% generated and guaranteed to be unique.  let's create a few more instances
% of Iterators...

t2 = Iterator('continuous', 'solver', 'ode45')
t3 = Iterator('continuous', 'solver', 'ode45')
t4 = Iterator('continuous', 'solver', 'ode45')

%% 
% You can see how that as we create more iterators, each of them has a
% unique name.  Now, let's create multiple state variables, each with their
% own iterator.  Remember, by giving a state its own iterator, it has its
% own timestep and its own solver, giving lots of flexibility to your
% model.

% create a state in a for loop
N = 10;
x = cell(1, N);
for i=1:N
    t = Iterator('continuous', 'solver', 'ode45');
    x{i} = mdl.state(0, 'iter', t); % set the initial value to zero and apply iterator t
end

% now, display the model
mdl

%% 
% The display of the model shows that there are 10 states and 10 iterators
% defined, each with it's own unique name.  But what if you needed to
% redefine the iterator?  We can so this by giving an iterator a specific name.
% We do that by passing in a string name as its first argument.
%
% Let's go through that example one more time:

% create the Model object again
mdl = Model('Scoping_of_Iterators');

% create a state in a for loop
N = 10;
x = cell(1, N);
for i=1:N
    t = Iterator('my_t', 'continuous', 'solver', 'ode45');
    x{i} = mdl.state(0, 'iter', t); % set the initial value to zero and apply iterator t
end

% now, display the model
mdl

% and see what 't' looks like
t

%%
% The Model display now shows that there is one iterator only in the
% system.  The name is left off since it's the only one.  Then, when we
% display the iterator, you can see that the iterator now has a very
% specific specified name, 'my_t'.
%
% In fact, this trick has other uses.  Sometimes, if the simEngine compiler
% returns an error, it returns the names that it knows of, the
% autogenerated names.  These autogenerated names can be come tricky to try
% to track down in your code.  Using an explicit name, like we did when we
% created 'my_t', can help track down bugs.  This trick works in other
% commands.  For example:

% first with an autogenerated name
x1 = mdl.state(0)
% now with an explicit name
x2 = mdl.state('x', 0)

% defining an equation with an autogenerated name
y1 = mdl.equ(x2^2)
% defining an equation with an explicit name
y2 = mdl.equ('y', x2^2)

%%
% You can definitely see the benefit of using explicit names when you write
% equations.

x1^2 + y1^2
x2^2 + y2^2

%%
% And when you use time referencing on an input

t1 = Iterator('continuous', 'solver', 'ode45');
t2 = Iterator('t', 'continuous', 'solver', 'ode45');

x1(t1-1)
x2(t2-1)

%% Additional Info
%
% A <simEngine_matlab_tutorial.html getting started guide> to using the
% MATLAB interface to DIESEL is the first place to go to learn how to model
% and simulate with simEngine.
%
% Several example models are packaged with simEngine.  These are in the
% examples directory, which you can easily find using the *simexamplepath*
% command in MATLAB.  A readme file in the examples directory tells you
% what each model is.
%
% There is also help available for all commands in the system.  Type 
% *help('command')* or *doc('command')* to find that information.  
% If you are looking
% for help on a method in model, type *help('Model/methodname')*.
%
% For any additional assistance, please contact our support team at
% support@simatratechnologies.com.
%
% Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
% Website: www.simatratechnologies.com
%