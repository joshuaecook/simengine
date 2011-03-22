%% simEngine Tutorial

%% Introduction
% Welcome, and thank you for using simEngine, the high-performance solution
% for nonlinear modeling in Matlab.  Our goal with simEngine is to create
% an incredibly user-friendly system for high-performance modeling of
% dynamical systems.  The purpose of this tutorial is to guide you through the basic
% concepts used to create models using simEngine's Matlab modeling
% framework, and to show you how to work
% with your models using the simex interface under Matlab.
%
% We recommend trying to type along with this guide, but if you want to use
% the Matlab files we've already created, you can find them in the
% tutorials directory underneath the simEngine examples directory (which
% you can easily access with the simexamplepath command).
%
%% Creating a model
% At its most basic level, a simEngine model is a software object created
% in Matlab by the *Model* class.  A model is created by a single call to
% the Model class constructor.

mdl = Model('myModel')

%%
% Here, you can see that a simEngine model object has been created.
% Currently, there are no inputs, outputs, nor states, but we'll add them
% soon.  There are also no sub-models, meaning that there is no other
% models contained within this model.  Finally, the solver is listed as
% using ode45 with a timestep of 0.1.  This is the default numerical
% integration method used in simEngine.
%
% Now, we can add some inputs to our model using the *input* method and see
% what happens:

input1 = mdl.input('input1');
input2 = mdl.input('input2');
input3 = mdl.input('input3');
mdl

%%
% We had to specify the input name in two places, as an argument to the
% *input* method and as a return value.  The arguments 'input1', 'input2',
% and 'input3', refer to how the input is going to be visible to other
% models and when simulating.  We'll see more of that shortly.  On the left
% hand side of the equation, *input1*, *input2*, and *input3* are now
% simEngine expression quantities that can be used as part of calculations.
%
% An output can be created in a similarly easy way.  For example, if we
% wanted an output that was a sum of the three inputs, we can create one by
% first computing the sum, and then creating an output of that sum.

total = input1 + input2 + input3
mdl.output(total);
mdl

%%
% The equation for *total* took the sum of the three arguments.  You can
% see that this calculation was performed symbolically, without using any
% numbers.  This is because *input1*, *input2*, and *input3* have not
% been defined yet.  The 'Exp:' that proceeds the expression is short for
% simEngine expression.  These expressions are saved and passed into
% simEngine for processing when you are ready to perform a simulation.
%
% Calling the *output* method with *total* created a new output called "total"
% and assigned it the symbolic expression of the sum of the three inputs.
% You can now see that the 'total' output has been added to the output
% list.
%
% This is a very simple example to show how inputs and outputs can be
% linked together.  The power of simEngine is evident once states are
% defined.  A state is a quantity that maintains its value throughout one
% iteration and is updated on each iterations.  After creating a state, you
% can define how it evolves over time using either differential or
% difference equations.  Let's start with a simple exponential decay
% function:

%%
% 
% $$x' = -k \cdot x$$
% 

%%
% This differential equation will exponentially decay towards zero assuming
% $k>0$.  To create this model in simEngine, we first create a *Model*
% object.

mdl = Model('ExponentialDecay');

%%
% Now, we can add an input, but this time, let's give it a default value
% in case we don't want to always specify it when we're ready to simulate
% the model.

k = mdl.input('k', 10); % define the default value of k to be 10.

%%
% Since there is one differential equation, we need to define a state
% variable _x_.  We do this with the *state* method.  There is always one
% required argument for *state*, the inital value of the state.  In this
% example, we'll make $x_0 = 20$.

x = mdl.state(20)
mdl

%%
% The above *state* method call, in a similar way to the *input* method
% call, returns a quantity that can be used in computations.  Since we
% didn't give it a name, the *state* method automatically gave it a unique
% name.  If we wanted to, we could have given the name 'x' to the state by
% calling the *state* method as: *x = mdl.state('x', 20)*;
%
% Now that we have created the state, we can define how the state evolved
% over time by providing a differential equations.  This is performed using
% the *diffequ* method.

mdl.diffequ(x, -k*x);

%%
% Finally, we want to be able to plot the evolution of the state variable
% _x_ over time, so we make it into an output.

mdl.output(x);
mdl

%%
% Viewing the model now shows that we now have one input named 'k', one
% output named 'x', a total of just one state, and no sub-models.
%

%% Simulating a model
%
% Now that a model is defined, it's easy to simulate it using simEngine.
% Simulations are performed by using the *simex* method that is available
% inside the Model object.
%
% To run a simulation, all we have to do is provide a stop time to *simex*,
% which will invoke simEngine to compile the model and perform the
% simulation.
% 

out = mdl.simex(1)
out.x

%%
% The result of the simulation is stored in the output structure *out*
% where each fieldname corresponds to a defined output of the model.
%
% Each output trace consists of a N by 2 matrix where the first column is
% time and the second column is the value of 'x'.
%
% We can plot this output using the standard Matlab plot command or by
% using the *simplot* short-hand command, which gives the identical
% results.

subplot(1,2,1);
plot(out.x(:,1), out.x(:,2));
subplot(1,2,2);
simplot(out.x);

%% Leaky membrane example
% Now we can combine what we've gone over above into generating a model
% complex model borrowed from computational neuroscience.
%
% The leaky membrane model is a simple model of a neuron without any active
% channels or conductances.  A neuron without any activity generally
% maintains a resting membrane potential of approximately -60 mV.  This
% models shows how the neuron can reach it's resting potential through a
% current leakage channel.

% First instantiate Model class to create a new Model object
mdl = Model('leakyMembrane');

% Define the method to be forward euler with a time step of 0.01 ms.
mdl.solver = 'forwardeuler';
mdl.dt = 0.01;

% Create an external input current
Iext = mdl.input('Iext', 0); % 0 nA is the default

% Define some constants
Cm = 0.5; % Membrane capacitance
gleak = 8; % Leakage channel conductance
Eleak = -60; % Nernst or reversal potential

% Define a voltage state
Vm = mdl.state(-45); % Initial membrane potential is set to -45 mV

% Create the leakage current
Ileak = gleak*(Vm-Eleak);

% Define the differential equation for Vm
mdl.diffequ(Vm, -(1/Cm)*(Ileak-Iext));

% Create the output
mdl.output(Vm);

%%
% Here, we've create a simple model for a leakyMembrane using the same
% basic components of model construction.  We added two additional
% properties above to designate a solver, in this case, forward-Euler, and
% a dt time step.  This solver is a first-order fixed time step method that
% can be computed very quickly.
%
% We can now simulate this model using the *simex* command as above.  This
% time, let's set the stop time to 1 ms and plot the results using
% *simplot*.

data = mdl.simex(1);
subplot(1,1,1);
simplot(data)

%%
% We can see that the output exponentially approaches the resting membrane
% potential of -60 mV from a starting value of -45 mV.
%
% When defining the model, we set _Iext_ to have a default value of 0 nA.
% We can easily change this by passing in an input structure to the *simex*
% command.  We can verify that this input does exist by simply displaying
% the model. Seeing that it is there, we can create a new structure called
% _input_ with the _Iext_ field set to 25 nA.

mdl
input.Iext = 25

%%
% Now, we can rerun the simulation from above, but this time with the new
% value for _Iext_.

data = mdl.simex(1, input);
simplot(data);

%%
% We see that with no input current, our leaky membrane model decays
% exponentially to -60, the value of *Eleak* in our model definition, which
% is exactly what it should do.   When we increase the amount of external
% current applied (that's what *Iext* is, after all), we don't decay as
% far.
% 
% What if we want to see how the model responds to several different values
% of *Iext*?  That's very easy to do using simex.  All we have to do is to
% specify *Iext* as a cell array of values instead of a scalar value, and
% simex will automatically run the model for all of those values:   

input.Iext = num2cell(0:100:1000)
data = mdl.simex(1, input)
simplot(data(:).Vm)

%%
% If you have simEngine Professional installed, you'll see that simEngine
% will automatically invoke the parallel compiler, and will execute all the
% simulations in parallel.

%% Saving and resuming simulations
% It's often useful to be able to save the final state of a model at the end
% of a simulation (a "snapshot" of the model's state space).  Using simex,
% we can easily save the final states and times of any simulations we run:  

input.Iext = 100;
[data1, finalState, finalTime] = mdl.simex(1, input);

%%
% Now, if we want to resume a simulation from that point, we simply pass
% the final state and time as input arguments to simex.

input.Iext = 0;
data2 = mdl.simex([finalTime finalTime+1], input, '-resume', finalState);
simplot(data1, data2); % plot both side by side

%%
% When we call simex this way, we're doing things slightly different than
% we did before.  First, we're handling the run time argument (the argument
% that comes after the model name) differently than we did before.  Before,
% when we specified run time, we gave only one value, saying "run for this
% much time with time starting at zero".  Now, we've given two arguments,
% saying "run from time *t1* to time *t2*".     
% 
% The second thing we've done differently here is to pass *finalState* into
% the simulation as the initial simulation time.  This says that the state
% values stored in *finalState* should be used as the initial conditions of
% the state variables in this simulation run.  This overrules the default
% value we specified in the Matlab model description.     

%% Creating more complex models
% In this section, we're going to talk about how to simplify some of your
% modeling tasks by using functions for commonly used expressions.
%
% Let's create a model a little more interesting than our leaky membrane
% model.  This code can be found in the examples/tutorial directory.

% Create a new model called 'neuron'
mdl = Model('neuron');
mdl.solver = 'forwardeuler';
mdl.dt = 0.01;

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
% This is a simple 3-state variable model of a neuron.  Even though it is a
% bit more complicated, we're not really doing anything different here that
% we haven't done with the leaky membrane model.
%
% Just like before, we can see what this model does by calling *simex*.

[data, finalState, finalTime] = mdl.simex(100);
simplot(data);

%%
% As before, we can easily run multiple simulations with different values
% of *Iext*, and use MATLAB to easily manipulate and analyze our data and
% produce interesting visualizations:

% perform the parallel simulation
input.Iext = num2cell(-50:2:100);
data = mdl.simex(100, input, '-resume', finalState);

% now visualize the results
for i = 1:76, 
   data3d(:,i) = data(i).Vm(1:5:end,2); 
end
figure, surf(data3d, 'EdgeAlpha', 0)


%%
% Let's try to clean this up some.  We can take advantage of Matlab's
% programming syntax to help us simplify our generated code.  For example,
% we can use Matlab's ability to define functions inline to simplify the
% equations, as in this set of equations with very similar structure:

mNaInf = 1/(1+exp((Vm+35)/-7.8));
hNaInf = 1/(1+exp((Vm+55)/7));
mKInf = 1/(1+exp((Vm+28)/-15));
hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16));
mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50));

%%
% In Matlab, we can define function, for example, one to compute the
% surface area of a cylinder in two ways, either in a block mode beginning
% with *function* keyword and ending with the *end* keyword, or in an
% inline way:

A = @(d, l)(2*pi*(d/2)^2 + pi*d*l);

%%
% Both of these methods work when the arguments are numeric or simEngine
% expressions generated from a Model object.


%%
% Here's how our neuron model looks when we use functions to replicate some
% of the frequently used expressions.

% Create a new model called 'neuron'
mdl = Model('neuron');
mdl.solver = 'forwardeuler';
mdl.dt = 0.01;

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
% This model is equivalent the our original neuron model, but by using
% functions, we've removed unnecessary intermediate equations and made our
% model description easier to read.  As you start building more complicated
% models with DIESEL, you'll find that using functions will help you to
% reduce modeling errors and accelerate your model development process.  
%
% We can also use sub-models to build more complex systems. The great power
% in simEngine is being able to take a complex piece of model code and
% easily re-use in a higher-level model.  Using this approach, it is very
% easy to build large, complex systems with little programming overhead.
% In order to bring a model into another model, we need to call one
% additional method, the submodel method inside a Model object.
% 
% Let's try building a simple two-cell neural network where one cell
% synapses onto the other.  To get started, we need to have two models, one
% of our neuron, and another our synapse.
%
% Our neuron is currently defined as _mdl_, but let's save that to a more
% descriptive variable name.

neuron_mdl = mdl;

% Now, let's create a synapse model in a similar way that we constructed
% the previous model.

synapse_mdl = Model('synapse');

% define the inputs to the model
Vpre = synapse_mdl.input('Vpre');
Vpost = synapse_mdl.input('Vpost');
gSyn = synapse_mdl.input('gSyn');
ESyn = synapse_mdl.input('ESyn');
Vthresh = synapse_mdl.input('Vthresh');

% evaluate the synaptic current using simEngine piecewise function to help
% define a conditional function
ISyn = piecewise(gSyn*(Vpost-ESyn),  Vpre > Vthresh, ...
...              % value             % condition 
...              % value             % condition
                 0                ); % otherwise

% return ISyn as an output
synapse_mdl.output(ISyn);

%%
% The piecewise expression above enables conditionals to be passed through
% and evaluated by simEngine during simulation.  If instead, a Matlab *if*
% statement was used, the *if* command would have been evaluated only once
% when the model was compiled and simEngine would now know of that
% conditional.  By using the *piecewise*, these conditional expressions
% will be evaluated at each iteration of the simulation.
%
% Now that we have a neuron and a synapse model, we can now define a two
% cell network model which will incorporate two neurons and one synapse

mdl = Model('twoCellNetwork');

%%
% Define the solver and the time step next. It is important to note here
% that any changes made to the solver in a higher-level model will trickle
% down to lower level models. So if we changed our solver here to *ode23*,
% then the neuron sub-models would be solved using *ode23*, even if
% *forwardeuler* is specified in _neuron_mdl_. 
mdl.solver = 'forwardeuler';
mdl.dt = 0.01;

% Create a stimulation current input
Istim = mdl.input('Istim', 0);

%%
% Here, we can define the submodels of this model.  We're going to add two
% neurons and one synapse.  This is done with the *submodel* method.  This
% takes in one argument, a *Model* object and returns an instance variable
% representing the submodel.

neuron1 = mdl.submodel(neuron_mdl);
neuron2 = mdl.submodel(neuron_mdl);
synapse1 = mdl.submodel(synapse_mdl);

%%
% If we remember, neuron_mdl had one input, _Iext_ and one output, _Vm_.
% Synapse had five inputs and one output, _ISyn_.  We can quickly see the
% inputs and outputs of each *Model* object by displaying the object or by
% calling the interface function.

[inputs, outputs] = interface(neuron_mdl)
[inputs, outputs] = interface(synapse_mdl)

%%
% Let's start assigning to these inputs and reading from these outputs.
% This is done by simply treating the inputs and outputs as properties of
% the instance variable.

% for neuron1
neuron1.Iext = Istim; % set this to the twoCellNetwork Istim input

% the synapse inputs
synapse1.Vpre = neuron1.Vm;
synapse1.Vpost = neuron2.Vm;
synapse1.gSyn = 1;
synapse1.ESyn = -60;
synapse1.Vthresh = -20;

% finally, neuron2's input
neuron2.Iext = -synapse1.ISyn; % negative by convention

%%
% Finish up the definition of this model by defining the outputs, but in this
% case, let's group the two outputs into one output.  This is useful if you
% would like multiple outputs organized together as one matrix, and if you
% don't want to separately save a time trace with each output.  When
% defining a grouped output, we must pass the output name, in this case, _Vm_, as a string since
% it does not previously exist.

mdl.output('Vm', neuron1.Vm, neuron2.Vm);

%%
% Let's go ahead and try running this model

data = simex(mdl, 100)
simplot(data);
legend('Vm1', 'Vm2');

%%
% Notice how data now includes a _Vm_ field with three columns,
% representing time, neuron1.Vm, and neuron2.Vm. And there we can see our
% two-neuron network, with neuron1's inhibitory synapse connected to
% neuron2.

%% Additional info
%
% Several example models are packaged with simEngine.  These are in the
% examples directory, which you can easily find using the *simexamplepath*
% command in MATLAB.  A readme file in the examples directory tells you
% what each model is.
%
% Also, take a look at the <simEngine_matlab_iterator_tutorial.html iterator tutorial> to see
% how iterators can be used to create mixed continuous and discrete
% systems, partition states across multiple solvers, and take advantage of
% streaming inputs.
%
% There is also help available for all commands in the system.  Type help
% _command_ or doc _command_ to find that information.  If you are looking
% for help on a method in model, type help or doc Model/_methodname_.
%
% For any additional assistance, please contact our support team at
% support@simatratechnologies.com.
%
% Copyright (c) 2010 Simatra Modeling Technologies, L.L.C.
% Website: www.simatratechnologies.com
%


