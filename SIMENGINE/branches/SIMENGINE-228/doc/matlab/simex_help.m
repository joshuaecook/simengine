%% simex
%
% Executes a high-performance software simulation engine using the
% simEngine compiler.
%
%% Syntax
%
%    MODEL = SIMEX(DSL)
%    [OUT, Y1, T1] = SIMEX(DSL, TIME, INPUTS, ...)
%
%% Description
%
% |MODEL = SIMEX(DSL)| compiles the model file |DSL| and returns a
% model description structure containing information
% which describes the model states, inputs, and outputs.
%
% |[OUT, Y1, T1] = SIMEX(DSL, TIME, ...)| compiles and executes a
% model over a given time interval and returns the simulations
% outputs and final states. The following parameters are required. 
% Additional optional parameters are
% listed below.
%
% |DSL| is the full pathname to a DIESEL model file.
%
% |TIME| is the simulation time limit. If scalar, it specifies a
% simulation starting at |T=0| proceeding to |T=TIME|. |TIME| must be
% greater than zero. |TIME| may be a 2-element array specifying a simulation
% starting at |T=TIME(1)| proceeding to |T=TIME(2)|. |TIME(2)| must
% be greater than |TIME(1)| and |TIME(1)| must be greater than or
% equal to zero.
%
%% Optional parameters
%
% |SIMEX(DSL, TIME, INPUTS, ...)| executes a model simulation with
% input values. |INPUTS| is a structure containing model input values. The
% field names of the structure correspond to model input names.
% The associated values may be scalar or they may be cell arrays of
% length _M_ indicating that _M_ parallel simulations are to be
% executed. All cell arrays must be the same size. In parallel
% simulations, scalar inputs are replicated for all models.
%
% |SIMEX(DSL, TIME, '-resume', Y0, ...)| resumes a model simulation
% from a starting state.  All state initial values specified within
% the model are ignored. |Y0| is an array of initial model state values. It should be
% the value returned in |Y1| from a previous execution. If
% parallel inputs are also given, the number of rows in |Y0| must
% be the same as the number of elements in the input cell array.
%
% |SIMEX(DSL, TIME, '-float', ...)| Constructs a simulation engine that computes in
% lower-precision floating point.
%
% |SIMEX(DSL, TIME, '-gpu', ...)| Constructs a parallel simulation engine capable of
% executing on a GPU.
%
%% Return values
%
% Up to three values are returned from a successful simulation.
%
% |OUT| is a structure array containing model output
% values. The structure array itself has length _M_ where _M_ is
% the number of parallel simulations. The field names of the
% structure correspond to model output names. The field
% values are two-dimensional matrices of varying sizes. The
% first dimension correlates to the time axis; the size of
% the second dimension is determined by the number of
% quantities specified in the model output.
%
% |Y1| is an M-by-N matrix of final state values where _N_ is the
% number of states in a model and _M_ is the number of parallel
% simulations.
%
% |T1| is a vector of length _M_ representing the time at which the
% simulation ended, where _M_ is the number of parallel simulations.
%   
%% Examples
% *Example #1*
%
% The Pyloric Dilator (PD) neural model from the lobster stomatogastric
% ganglion is specified to have 13 states, eight parameterized inputs and one voltage
% output.  The output interface includes the names, plus any initial values
% if specified.  We can compile a DIESEL model and return a data structure
% representing the model interface using SIMEX.

PD = simex([simexamplepath '/PD/pd.dsl'])

%%
% We can execute this model by running the SIMEX command with a stop time
% specified.
%

outputs = simex([simexamplepath '/PD/pd.dsl'], 1500)
figure
simplot(outputs)

%%
% We can rerun the same simulation, changing the inputs by pulling the
% original input values out of the interface, and adjusting them
% individually.

inputs = PD.defaultInputs
inputs.gA = 20;
outputs_increased_gA = simex([simexamplepath '/PD/pd.dsl'], 1500, inputs);
figure
simplot(outputs, outputs_increased_gA)
legend(['gA = ' num2str(PD.defaultInputs.gA)], ...
       ['gA = ' num2str(inputs.gA)]);

%%
% We can quickly simulate and plot a range of inputs by creating a cell
% array of input values.

inputs.gA = num2cell(0:10:50)
output_array = simex([simexamplepath '/PD/pd.dsl'], 1500, inputs);

%%
% The generated output array is an array of structures

output_array

%% 
% Plotting the output looks like:
figure;
for i=1:length(output_array)
    subplot(2,3,i)
    simplot(output_array(i));
    title(['gA = ' num2str(inputs.gA{i})])
end

%% 
close all

%% See also
% <simexamplepath_help.html
% |simexamplepath|>, <simplot_help.html |simplot|>

%% 
% Copyright 2009,2010 Simatra Modeling Technologies, L.L.C.
%
% For more information, please visit
% <http://www.simatratechnologies.com>
%
% For additional help, please email
% <mailto:support@simatratechnologies.com support@simatratechnologies.com>
