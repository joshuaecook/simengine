%% simex
% Executes a high-performance software simulation engine using the
% simEngine compiler.
%
%
%% Description
%     SIMEX compiles the model defined in the DSL file into a
%     high-performance software simulation engine.
%
%% Usage
%     OUT = SIMEX(DSL, TIME, ...) accepts the following options, all
%     of which are optional except for the file name of the DSL model and the
%     time to execute the simulation.
%
%       DSL is a full pathname to a DSL model file.
%
%       TIME is the simulation time limit. If scalar, it specifies a
%       simulation starting at T=0 proceeding to T=TIME. TIME must be
%       greater than zero.
%
%       TIME may be a 2-element array specifying a simulation
%       starting at T=TIME(1) proceeding to T=TIME(2). TIME(2) must
%       be greater than TIME(1) and TIME(1) must be greater than
%       zero.
%
%       INPUTS is a structure containing model input values. The
%       field names of the structure correspond to model input names.
%       The associated values may be scalar or they may be cell arrays of
%       length M indicating that M parallel simulations are to be
%       executed. All cell arrays must be the same size. In parallel
%       simulations, scalar inputs are replicated for all models.
%
%     [OUT, STATES] = SIMEX(DSL, TIME, ...) returns the final state vector
%     computed along with the outputs
%
%     [OUT, STATES, TF] = SIMEX(DSL, TIME, ...) returns the final time
%     value reached by the simulator
%
%       Up to three values are returned:
%
%       OUT is a structure array containing model output
%       values. The structure array itself has length M where M is
%       the number of parallel simulations. The field names of the
%       structure correspond to model output names. The field
%       values are two-dimensional matrices of varying sizes. The
%       first dimension correlates to the time axis; the size of
%       the second dimension is determined by the number of
%       quantities specified in the model output.
%
%       Y1 is an M-by-N matrix of final state values where N is the
%       number of states in a model and M is the number of parallel
%       simulations.
%
%       T1 is a vector of length M representing the time at which the
%       simulation ended, where M is the number of parallel simulations.
%
%     MODEL = SIMEX(DSL) compiles DSL as above and returns a
%     model description structure containing information
%     which describes the model states, inputs, and outputs.
%
%% Optional Parameters
%
%       '-resume'
%         The simulation will continue from the initial state vector
%         specified in Y0. All state initial values specified within
%         the model are ignored.
%
%         Y0 is an array of initial model state values. It should be
%         the value returned in Y1 from a previous execution. If
%         parallel inputs are given, the number of rows in Y0 must
%         be the same as the number of elements in the input cell array.
%
%       '-float'
%         Constructs a simulation engine that computes in
%         lower-precision floating point.
%
%       '-gpu'
%         Constructs a parallel simulation engine capable of
%         executing on a GPU.
%
%   
%% Examples
% *Example #1*
%
% The Pyloric Dilator (PD) neural model from the lobster stomatogastric
% ganglion is specified to have 13 states, eight parameterized inputs and one voltage
% output.  The output interface includes the names, plus any initial values
% if specified.  We can compile a DSL model and return a data structure
% representing the model interface using SIMEX.

interface = simex([simexamplepath '/PD/pd.dsl'])

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

inputs = interface.defaultInputs
inputs.gA = 20;
outputs_increased_gA = simex([simexamplepath '/PD/pd.dsl'], 1500, inputs);
figure
simplot(outputs, outputs_increased_gA)
legend(['gA = ' num2str(interface.defaultInputs.gA)], ...
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
% <simplot_help.html |simplot|>,  <simexamplepath_help.html
% |simexamplepath|>

%% 
% Copyright 2009,2010 Simatra Modeling Technologies, L.L.C.
% For more information, please visit <http://www.simatratechnologies.com>
% For additional help, please email <mailto:support@simatratechnologies.com>
