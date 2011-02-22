%% simengine_tutorial
% A tutorial for getting started with simEngine
%
%% Introduction
% Welcome, and thank you for using simEngine, the high-performance solution
% for nonlinear modeling in Matlab.  Our goal with simEngine is to create
% an incredibly user-friendly system for high-performance modeling of
% dynamical systems.  The purpose of this tutorial is to guide you through the basic
% concepts used to create models using simEngine's DIESEL modeling
% language, and to show you how to work
% with your models using the simex interface under Matlab.
%
% Before we get started, let's go over the formatting of this guide, so it
% will be clear what's going on.  Normal text looks just like what you're
% reading right now.  Text that is meant to be entered as part of a DIESEL
% model description is presented with a pale yellow
% background, like this:
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
% model (out) = MyModel(input1, input2, input3)<br> 
% ...<br>
% end
% </td></tr></table>
% </html>
%
% Text that is meant to be entered in Matlab, and the results displayed by
% Matlab, are shown like this:

x = 1:10

%%
% We recommend trying to type along with this guide, but if you want to use
% the DIESEL files we've already created, you can find them in the
% tutorials directory underneath the simEngine examples directory (which
% you can easily access with the simexamplepath command).  

%% Creating a model with DIESEL
%
% At its most basic, a DIESEL model is composed of 5 parts:  the model
% definition, constants, states, equations, and a solver.  Let's go
% over each of these one-by-one so we can understand what each one does.
%
% *1. Model definition.*
%
% The model definition line is the most important line in a DIESEL model
% description.  The model definition line is where we give our DIESEL model
% a name and describe the inputs and outputs of the model.  The DIESEL
% keyword model tells us that we're defining a new model.  After the model
% keyword comes a list of the model's outputs in parentheses, the model's
% name, and a list of the model's inputs in parentheses.  For example:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%         model (output1, output2) = modelName(input1, input2, input3)
% </td></tr></table>
% </html>
%
% A couple of other important points to know about the model definition
% line.  First, any model that we're going to run using the simex interface
% (see the next section) needs to have the same name as the .dsl filename.
% For example, if our .dsl file is called "cell.dsl", our model definition
% line would need to use "cell" for the model name:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%         model (output1, output2) = cell(input1, input2, input3)
% </td></tr></table>
% </html>
% 
% Second, the input and output lists can contain any number of inputs or
% outputs, but they must be contained in parentheses, even if there's only
% one input or output.  For example, if our cell model had one input named
% "current" and one output named "voltage", it would look like this:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     model (voltage) = cell(current)
% </td></tr></table>
% </html>
% 
% Third, an input or an output must be defined on the model definition line
% in order to be used in the model.  If you don't define an output on the
% model definition line, it can't be seen by higher-level models or the
% simulation interface.  If you don't define an input on the model
% definition line, it can't be controlled by higher-level models or the
% simulation interface.     
% 
% Finally, the model keyword must be matched by an end keyword at the end
% of the model: 
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     model (voltage) = cell(current)<br>
%         ... model equations, etc. go here ...<br>
%     end
% </td></tr></table>
% </html>
% 
% Model definition also includes specifying the properties of model inputs
% and outputs.  To do that, we just use the *input* and *output* keywords,
% like this:
%
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     model (voltage) = cell(current)<blockquote>
%         input current with {default = 0}<br>
%         output voltage = current*resistance</blockquote>
%     end
% </td></tr></table>
% </html>
%
% *2. Constants*
%
% Constants are the simplest data type in a DIESEL model.  Constants are
% assigned a value in the .dsl file and they don't change unless you change
% the file.  To define a constant, use the constant keyword:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     constant pi = 3.1415926
% </td></tr></table>
% </html>
%
% (note that we didn't really have to define pi, because its part of the
% standard DIESEL library, but it works as a good illustrative example).  
% 
% *3. States*
% 
% States are the dynamical variables of the model.  States can be
% associated with a differential equation (see "equations" below).  When we
% define a state, we use the state keyword and provide a default initial
% value for the state.    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     state x = 1
% </td></tr></table>
% </html>
% 
% 
% With this line, we're declaring that "x" is going to be a state of our
% system, and that its default initial value is going to be 1.  
% 
% *4. Equations*
%
% Equations are where most of the action is in a model description.
% Equations can be either _intermediate equations_, which combine inputs,
% constants, states, and other intermediates into a single expression, or
% _differential equations_, which must be associated with a state variable.
% Both differentials equations and intermediate equations are defined using
% the equation keyword.  For example:     
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     equation area = pi*radius^2
% </td></tr></table>
% </html>
% 
% creates an intermediate equation describing the area of a circle. 
% 
% Differential equations are described by placing the ' symbol after a
% declared state variable.  The following line:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     equation x' = -2*x + 3
% </td></tr></table>
% </html>
% 
% Creates a differential equation for the state variable x that says that
% the first derivative of x with respect to time is equal to -2x +3.   
% 
% Equations can also be described using an equation block.  An equation
% block can contain both intermediate equations and differential equations.
% To create an equation block, use the equations keyword and a terminating
% end keyword:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%         equations<blockquote>
%             area = pi*radius^2<br>
%             x' = -2*x+3</blockquote>
%         end<br>
% </td></tr></table>
% </html>
% 
% *5. Solver*
%
% The solver keyword is used to specify what method we will use to solve
% the differential equations in the model.  To define a solver, use ‘solver
% =' followed by the name of the solver:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     solver = forwardeuler
% </td></tr></table>
% </html>
% 
% The types of solvers that are currently supported in simEngine are:
% forwardeuler, ode23, ode45, rk4, and cvode.  The solver definition can be
% followed with options to set properties of the solver.  For example, the
% time step for fixed-step solvers can be specified using the "dt" option:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     solver = forwardeuler<br>
%     solver.dt = 0.02
% </td></tr></table>
% </html>
% 
% More information about the different ODE solvers available with simEngine
% are provided in Part V of this guide.  
%
% *Putting it all together.*
%
% Now that we've gone over the five basic parts of a model, let's put it
% all together.  Create a new file called "leakyMembrane.dsl" using your
% favorite text editor and enter the following lines:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%   model (Vm) = leakyMembrane(Iext)<blockquote>
%   input Iext with {default = 0}<br><br>
%   constant Cm = 0.5<br>
%   constant gleak = 8<br>
%   constant Eleak = -60<br><br>
%   state Vm = -45<br><br>
%   equations<blockquote>
%     Ileak = gleak*(Vm - Eleak)<br>
%     Vm' = -(1/Cm)*(Ileak - Iext)</blockquote>
%   end<br><br>
%   solver = forwardeuler<br>
%   solver.dt = .01</blockquote>
% end
% </td></tr></table>
% </html>
% 
% Here, we've created a simple model called "leakyMembrane", using the five
% basic components of model construction.  As we go on, you'll see how we
% can use these basic components (and a few others) to easily build
% sophisticated models using DIESEL.  But for now, let's turn our attention
% to how we run our DIESEL models using the simex interface in Matlab.    
%% Running simulation with simex
%
% *Using simex to get model information.*
% simex is an interface between matlab and the simEngine compiler.  simex
% automatically handles all the details of compilation, from specifying the
% hardware target to recording model data.  In order to use simex, start up
% Matlab and change into whatever directory you saved "leakyMembrane.dsl".
%
% First, we'll use simex extract the model data by calling simex on our
% DIESEL model file "leakyMembrane.dsl" without any additional arguments.

modelInfo = simex([simexamplepath '/tutorial/leakyMembrane.dsl'])

%%
% When we call simex without any additional arguments, we get a structure
% like the one above
% that describes the various properties of the model, such as the names and
% default values of the model inputs, outputs, and states.
%
% *Using simex to run a model with different inputs.*
%
% In order to actually run the model, we need to call simex with the model
% name and the amount of time we want to run the model for: 
% 

data = simex([simexamplepath '/tutorial/leakyMembrane.dsl'], 1)

%% 
% This command tells simEngine to run the model in "leakyMembrane.dsl" for
% a time of 1 (with whatever units we used to describe the model), and the
% results are placed into the structure "data".  
% The structure "data" has a field called "Vm" (the output of our
% leakyMembrane model) that has two colums.  The first column is the
% simulation time, and the second column is the value of Vm at those points
% in time.  We can easily plot this data using matlab's plot command, or
% the simplot command included with simex.  Both of these commands will
% produce the equivalent plot:
% 

plot(data.Vm(:,1), data.Vm(:,2))
simplot(data)

%%
% How 
% about we try changing the value of Iext, the input to our model. Its
% very easy to set parameters and input values using  
% simex.  First, we copy
% the default input values from modelInfo into a structure called
% parameters, and then we can set a value for Iext:     
% 

parameters = modelInfo.defaultInputs;
parameters.Iext = 25;

%% 
% Now that we have a real value for Iext, we can run simex using our
% parameters structure: 
% 

data = simex([simexamplepath '/tutorial/leakyMembrane.dsl'], 1, parameters);
hold on, simplot(data, 'r')

%% 
% We see that with no input current, our leaky membrane model decays
% exponentially to -60, the value of Eleak in our model definition, which
% is exactly what it should do.   When we increase the amount of external
% current applied (that's what Iext is, after all), we don't decay as far.
% 
% What if we want to see how the model responds to several different values
% of Iext?  That's very easy to do using simex.  All we have to do is to
% specify Iext as a cell array of values instead of a scalar value, and simex
% will automatically run the model for all of those values:   
% 

parameters.Iext = num2cell(0:100:1000);
data = simex([simexamplepath '/tutorial/leakyMembrane.dsl'], 1, parameters);
figure, hold on
for i = 1:11,
     simplot(data(i))
end

%%
% If you have simEngine professional installed, you'll see that simEngine
% will automatically invoke the parallel compiler, and will execute all the
% simulations in parallel.
%
% *Using simex to save and resume from a particular model state.*
%
% Its often useful to be able to save the final state of a model at the end
% of a simulation (a "snapshot" of the model's state space).  Using simex,
% we can easily save the final states and times of any simulations we run:  
% 

parameters.Iext = 100;
[data1 finalState finalTime] = simex([simexamplepath '/tutorial/leakyMembrane.dsl'], ...
    1, parameters);

%% 
% If we want to resume a simulation from that point, we simply pass the final state and time as input arguments to simex:
% 

parameters.Iext = 0;   
data2 = simex([simexamplepath '/tutorial/leakyMembrane.dsl'], [finalTime finalTime+1], ...
    parameters, '-resume', finalState);
figure, hold on, simplot(data1), simplot(data2,'r') 

%% 
% When we call simex this way, we're doing things slightly different than
% we did before.  First, we're handling the run time argument (the argument
% that comes after the model name) differently than we did before.  Before,
% when we specified run time, we gave only one value, saying "run for this
% much time with time starting at zero".  Now, we've given two arguments,
% saying "run from time t1 to time t2".     
% 
% The second thing we've done differently here is to pass finalState into
% the simulation as the initial simulation time.  This says that the state
% values stored in "finalState" should be used as the initial conditions of
% the state variables in this simulation run.  This overrules the default
% value we specified in the DIESEL model description.     
%% Using functions to simplify common expressions 
% In this section, we're going to talk about how to simplify some of your
% modeling tasks by using
% functions for commonly used expressions.  
% 
% Let's start by trying something a little more interesting than our leaky membrane
% model.  Create a file called "neuron.dsl" and enter the following lines
% (or just copy the neuron.dsl file out of the examples/tutorial
% directory): 
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%  model(Vm) = neuron(Iext)<blockquote>
%  input Iext with {default = 0}<br><br>
%  constant Cm = 1<br><br>
%  constant gNa = 120<br>
%  constant gK = 100<br>
%  constant gleak = .51 <br><br>
%  constant ENa = 55<br>
%  constant EK = -80<br>
%  constant Eleak = -55<br><br>
%  state Vm = -45<br>
%  state hNa = 0.9<br>
%  state mK = 0.1<br><br>
%  equations<br>
% <blockquote>
%      mNaInf = 1/(1+exp((Vm+35)/-7.8))<br>
%      hNaInf = 1/(1+exp((Vm+55)/7))<br>
%      mKInf = 1/(1+exp((Vm+28)/-15))<br><br>
%      hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16))<br>
%      mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50))<br><br>
%      INa = gNa*mNaInf^3*hNa*(Vm - ENa)<br>
%      IK = gK*mK^4*(Vm - EK)<br>
%      Ileak = gleak*(Vm - Eleak)<br><br>
%      hNa' = (hNaInf - hNa)/hNaTau<br>
%      mK' = (mKInf - mK)/mKTau<br>
%      Vm' = -(1/Cm)*(INa + IK + Ileak -Iext)<br>
% </blockquote>
%  end<br><br>
%  solver = forwardeuler<br>
%  solver.dt = .001<br><br></blockquote>
% end
% </td></tr></table>
% </html>
% 
% This is simple 4-state variable model of a neuron.  Even though its a bit
% more complicated, we're not really 
% doing anything new here that we didn't do with the leaky membrane model. 
% 
% Let's see what this model does:
% 

[data finalState finalTime] = simex([simexamplepath '/tutorial/neuron.dsl'], 100);
simplot(data)

%% 
% As before, we can easily run multiple simulations with different values of Iext, and use Matlab to easily manipulate and analyze our data and produce interesting visualizations:
%

parameters.Iext = num2cell(-50:2:100);
data = simex([simexamplepath '/tutorial/neuron.dsl'], 100, ...
parameters, '-resume', finalState);
for i = 1:76, 
   data3d(:,i) = data(i).Vm(1:5:end,2); 
end
figure, surf(data3d, 'EdgeAlpha', 0)

%%
% You may have noticed that some parts of our neuron model have a great
% deal of similarity: 
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%          mNaInf = 1/(1+exp((Vm+35)/-7.8))<br>
%          hNaInf = 1/(1+exp((Vm+55)/7))<br>
%          mKInf = 1/(1+exp((Vm+28)/-15))<br><br>
%          hNaTau = 30/(exp((Vm+50)/15)+exp(-(Vm+50)/16))<br>
%          mKTau = 7/(exp((Vm+40)/40)+exp(-(Vm+40)/50))<br>
% </td></tr></table>
% </html>
%
% Let's try to clean this up some.  In a DIESEL model we can create
% functions to handle commonly used expressions.  The *function* keyword is
% used to define a new function.  A function definition looks a lot like a
% model definition, and takes the following format:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     function functionName(inputs) = expression
% </td></tr></table>
% </html>
% 
% Where functionName is the name of the function, inputs is a list of
% inputs, and expression is the math that the function reproduces.  For
% example, a function to calculate the surface area of a cylinder would
% look like this:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     function area(d, l) = 2*pi*(d/2)^2 + pi*d*l
% </td></tr></table>
% </html>
% 
% where _d_ and _l_ are the diameter and the length of the cylinder,
% respectively.  
% 
% Here's how our neuron model looks when we use functions to replicate some of the frequently used expressions:
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     model (Vm) = simplifiedNeuron(Iext)<br><blockquote>
%           function xinf(a, b) = 1/(1+exp((Vm + a)/b))<br>
%           function tau(a, b, c, d) = a/(exp((Vm+b)/c) +
%           exp(-(Vm+b)/d))<br><br>
%           input Iext with {default = 0}<br><br>
%           constant Cm = 1<br>
%           constant gNa = 120<br>
%           constant gK = 100<br>
%           constant gleak = .51<br>
%           constant ENa = 55<br>
%           constant EK = -80<br>
%           constant Eleak = -55<br><br>
%           state Vm = -45<br>
%           state hNa = 0.9<br>
%           state mK = 0.1<br><br>
%           equations   
%            <blockquote>
%              INa = gNa*xinf(35, -7.8)^3*hNa*(Vm - ENa)<br>
%              IK = gK*mK^4*(Vm - EK)<br>
%              Ileak = gleak*(Vm - Eleak)<br><br>
%              hNa' = (xinf(55, 7) - hNa)/tau(30, 50, 15, 16)<br>
%              mK' = (xinf(28, -15) - mK)/tau(7, 40, 40, 50)<br>
%              Vm' = -(1/Cm)*(INa + IK + Ileak - Iext)
%            </blockquote>
%           end<br><br>
%           solver = forwardeuler<br>
%           solver.dt = .001<br></blockquote>
%     end<br>
% </td></tr></table>
% </html>
% 
% 
% This model (which is in the file simplifiedNeuron.dsl) is equivalent to
% our original neuron model, but by using functions, we've removed
% unnecessary intermediate equations and made our model description easier
% to read.  As you start  
% building more complicated models with DIESEL, you'll find that using
% functions will help you to reduce modeling errors and accelerate your
% model development process.    
%% Using sub-models to build complex systems
% There's one last topic to cover before we end our getting started guide.
% The great power in simEngine is being able to take a complex piece of
% model code and easily re-use in a higher-level model.  Using this
% approach, it is very easy to build large, complex systems with little
% programming overhead.  In order to bring a model into another model, we
% need to use two commands.  The first is the import statement at the top of
% a .dsl file and the second is the submodel keyword within a model
% description.       
% 
% Let's try building a simple two-cell neural network where one cell
% synapses onto the other.  To get started, we'll need to have models for
% both our neuron and a synapse save your "simplifiedNeuron.dsl" file into
% a new file called "neuronWithSynapse.dsl", and edit to so that it looks
% like following:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     function xinf(a, b, V) = 1/(1+exp((V + a)/b))<br>
%     function tau(a, b, c, d, V) = a/(exp((V+b)/c) +
%     exp(-(Vm+b)/d))<br><br>
%     model (Vm) = neuron(Iadd, Iext)<blockquote>
%           input Iext with {default = 0}<br>
%           input Iadd with {default = 0}<br><br>
%           constant Cm = 1<br><br>
%           constant gNa = 120<br>
%           constant gK = 100<br>
%           constant gleak = .51<br><br>
%           constant ENa = 55<br>
%           constant EK = -80<br>
%           constant Eleak = -55<br><br>
%           state Vm = -45<br>
%           state hNa = 0.9<br>
%           state mK = 0.1<br><br>
%           equations     <blockquote>
%              INa = gNa*xinf(35, -7.8, Vm)^3*hNa*(Vm - ENa)<br>
%              IK = gK*mK^4*(Vm - EK)<br>
%              Ileak = gleak*(Vm - Eleak)<br><br>
%              hNa' = (xinf(55, 7, Vm) - hNa)/tau(30, 50, 15, 16, Vm)<br>
%              mK' = (xinf(28, -15, Vm) - mK)/tau(7, 40, 40, 50, Vm)<br>
%              Vm' = -(1/Cm)*(INa + IK + Ileak+ Iadd -
%              Iext)</blockquote>
%           end<br><br>
%           solver = forwardeuler<br>
%           solver.dt = .001</blockquote>
%     end<br><br>
%     model (ISyn) = synapse(Vpre, Vpost, gSyn, ESyn, Vthresh)<blockquote>
%           equation ISyn = {(gSyn*(Vpost-ESyn)) when Vpre > Vthresh,
%                            0 otherwise}<br></blockquote>
%     end
% </td></tr></table>
% </html>
% 
% You'll notice we've done a couple of things differently here.  First,
% we've moved our xinf and tau functions outside of the neuron model
% definition block and changed their format slightly.  You'll see why we
% did this in next section.  Next, you'll notice that we have two different
% model definitions in the same file: neuron and synapse.  A single .dsl
% file can have as many model definition blocks as you want, although only
% the _top-level model- that matches the file name can be compiled using simex.   
% 
% You also may have noticed this line in the model definition for synapse:
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     equation ISyn = {(gSyn*(Vpost-ESyn)) when Vpre > Vthresh,
%                      0                   otherwise}
% </td></tr></table>
% </html>
%
% This line looks different from equations that we've seen before, because
% it contains a conditional expression.  A conditional expression let's you
% give an equation different values depending on what is going on within
% the model.  The format of a conditional expression uses curly braces and
% the *when* and *otherwise* keywords, like this:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     { value1 when condition1,
%       value2 when condition2,
%        ...
%       value otherwise}
% </td></tr></table>
% </html>
% 
% A conditional expression can have as many value-condition pairs as you
% want. 
% 
% Finally, even though our file is called "neuronWithSynapse.dsl", we don't
% have any models called "neuronWithSynapse" defined.  How can we get away
% with this?  Its because we're not going to use these model with the simex
% interface (which requires us to have a model name that matches the file
% name), we're going to use these models inside of another model using the
% import statement.  Let's see how this works.  Create a file called
% "twoCellNetwork.dsl" and enter the following model definition:      
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     import "neuronWithSynapse.dsl"<br><br>
%     model (Vm1, Vm2) = twoCellNetwork(IStim)<blockquote>
%         input IStim with {default = 0}<br>
%         submodel neuron neuron1 with {Iext = IStim}<br>
%         submodel neuron neuron2<br><br>
%         submodel synapse synapse1 with {Vpre = neuron1.Vm, Vpost =
%         neuron2.Vm,<br>
%                     gSyn = 1, ESyn = -60, Vthresh = -20}<br><br>
%         neuron2.Iadd = synapse1.ISyn<br><br>
%         output Vm1 = neuron1.Vm<br>
%         output Vm2 = neuron2.Vm<br><br>
%        solver = forwardeuler<br>
%         solver.dt = .001</blockquote>
%     end
% </td></tr></table>
% </html>
% 
% Let's look at what's going on here.  The first line reads:
%
% <html>
% <table bgcolor="F0E68C">
% <tr><td>
%     import "neuronWithSynapse.dsl"
% </td></tr></table>
% </html>
% 
% Here, we're telling simEngine to import the models and functions
% described in the file "neuronWithSynapse.dsl" into the two-cell network model.
% That means that we can create new instances of the neuron model we
% originally developed inside our new network model.  After we've imported
% all of the sub-model definitions we're going to use, we can then move on
% to our definition of the network model:     
%
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     model (Vm1, Vm2) = twoCellNetwork(IStim)
% </td></tr></table>
% </html> 
% 
% Not much is different about the model definition line, except that now we
% have two outputs defined, Vm1 and Vm2, instead of just one output.  The
% next two lines are where we create a sub-model of a neuron, using the
% *submodel* keyword:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     submodel neuron neuron1 with {Iext = IStim}
%     submodel neuron neuron2
% </td></tr></table>
% </html>
% 
% Because we imported the "neuron.dsl" file, we can now create sub-models
% out of the neuron model that was defined in neuron.dsl.  We create
% sub-models using the *submodel* keyword.  The submodel keyword is followed
% by the model type (in this case "neuron", because that is the name of the
% model we imported) and the name of the submodel ("neuron1" and "neuron2"
% for our two instances of the "neuron" model).  With neuron1, we're also
% going to use the with { ...} syntax to set neuron1's Iext input to the
% "IStim" input of our two-cell network model.  We make even more extensive
% use of the with {...} syntax when we define our synapse sub-model:        
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     submodel synapse synapse1 with {Vpre = neuron1.Vm, Vpost = neuron2.Vm,
%                     gSyn = 1, ESyn = -60, Vthresh = -20}
% </td></tr></table>
% </html>
% 
% Here, we're saying that the presynaptic potential is linked to Vm of
% neuron1, and the post-synaptic potential is linked to the Vm of neuron2.
% How do we actually introduce the synaptic current produced by the synapse
% model into one of the neurons?  That comes in the next line:   
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     neuron2.Iadd = synapse1.ISyn
% </td></tr></table>
% </html>
% 
% Here, we're taking the ISyn output of synapse1 and setting it to the Iadd
% input of neuron2.  Now that we've created our simple network, we just
% need to define our outputs, Vm1 and Vm2.  We can do this using the output
% keyword:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     output Vm1 = neuron1.Vm
%     output Vm2 = neuron2.Vm
% </td></tr></table>
% </html>
% 
% On the model definition line, we created two outputs for model named
% "Vm1" and "Vm2".  By using the output keyword, we're defining what the
% Vm1 and Vm2 outputs actually are.  In this case, we're linking them to
% the Vm outputs of neuron1 and neuron2, respectively.  As with our other
% models, we end the definition block with our solver and solver properties:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     solver = forwardeuler
%     solver.dt = 0.01
% </td></tr></table>
% </html>
% 
% These lines are just like we used before to specify the solver and the
% solver dt.  It is important to note here that any changes made to the
% solver in a higher-level model will trickle down to lower level models.
% So if we changed our solver here to ode23, then the neuron sub-models
% would be solved using ode23, even if forwardeuler is specified in
% neuron.dsl.     
% 
% Go ahead and try running this model:
% 

m = simex([simexamplepath '/tutorial/twoCellNetwork.dsl'], 100);
figure, simplot(m)
legend({'Vm1', 'Vm2'}) 

%%
% And there we can see our two-neuron network, with neuron1's inhibitory
% synapse connected to neuron2. 
% 
% *Making changes with submodels, and using comments.*
% Often, you may want to change the property of a sub-model without
% changing the higher-level or top-level model that it links to.  That's
% very easy to do with simEngine.  The synapse model we used in the
% previous example was fairly simple.  Let's open up neuronWithSynapse.dsl
% and make the following changes:    
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     model (ISyn) = synapse(Vpre, Vpost, gSyn, ESyn, Vthresh)<blockquote>
%         state m = 0.1<br><br>
%         constant tau = 0.5<br><br>
%         equation minf = xinf(28, -15, Vpre)<br>
%         equation m' = (minf - m)/tau<br>
%         equation ISyn = {(gSyn*m*(Vpost-ESyn)) when Vpre > Vthresh,<br>
%                             0 otherwise}</blockquote>
%     end 
% <blockquote>
%     /*model (ISyn) = synapse(Vpre, Vpost, gSyn, ESyn, Vthresh)<br>
%           equation ISyn = {(gSyn*(Vpost-ESyn)) when Vpre > Vthresh,<br>
%                                  0 otherwise}<br>
%     end*/
% </blockquote>
% </td></tr></table>
% </html>
%
% Now we've added a state variable to the synapse model in order to give it
% some more complex dynamics.  Remember how we moved the xinf function
% outside of the neuron model block?  Because we defined the xinf function
% outside of a model block and at the start of the file, the xinf function
% is available to all models defined in neuronWithSynapse.dsl, so we can
% easily use the xinf function in our new synapse model.     
% 
% We've also "saved" our old synapse definition by by using comments.
% DIESEL uses C-style comments, which means you can specify a comment two
% ways:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     //like this for a single line comment
% </td></tr></table>
% </html>
% 
% or
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     /*for a multiple-line comment<br>
%       such as this<br>
%       you can do it like this*/
% </td></tr></table>
% </html>
%
% Go ahead and run simex on twoCellNetwork.dsl.  The new synapse definition
% will automatically be included when the model is re-compiled.
% 
%% Additional things to try.
% Now you've learned about the basics of building models using DIESEL, and
% running them with the simex interface to Matlab.  There's a lot more that
% you can do with simEngine, but in the end, everything comes back to the
% basics you've learned in this lesson.  Here's some additional ideas of
% things to try:    
% 
% *Find out more about simEngine*
%
% From the Matlab prompt, type simhelp.  This command will print out all
% the of the various commands that are available with simEngine in Matlab  
% 
% *Explore other example models*
%
% Several example models are packaged with simEngine.  These are in the
% examples directory, which you can easily find using the simexamplepath
% command in matlab.  A readme file in the examples directory tells you
% what each model is.  You can even use this command as a shortcut to
% running models in the examples directory:    
%
% *Make outputs conditional*
%
% Sometimes you only want to output something from a model when certain
% conditions are met.  You can do this by specifying a condition using the
% with {...} syntax after the output keyword:  
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     output z with {condition = z > 0}
% </td></tr></table>
% </html>
% 
% In the above, the output z will only be recorded when it is greater than
% zero. 
% 
% *Using additional solvers*
%
% The following solvers are available in the 0.9 release of simEngine:
% 
%     * forwardeuler, the standard 1st-order, fixed dt explicit euler method
%     * rk4, a 4th-order, fixed dt method
%     * ode23,  Bogacki-Shampine, variable dt method
%     * ode45, Dormand-Prince, variable dt method
%     * cvode, implicit, stiff variable dt method
% 
% 
% You can specify different properties of the solvers by using the with
% {...} syntax.  The properties you can specify are: 
% 
%     * dt, time step for fixed time step solvers, or initial time step to
%     try for variable time step solvers 
%     * reltol, relative tolerance for variable time step solvers
%     * abstol, absolute tolerance for variable time step solvers
% 
% 
% For example:
% 
% <html>
% <table bgcolor="F0E68C">
% <tr><td> 
%     sovler = ode45 with {dt = 1, reltol = 1e-3, abstol = 1e-6}<br>
%     solver = rk4 with {dt = 1e-2}
% </td></tr></table>
% </html>

