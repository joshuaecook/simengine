%SIMSWEEP   Executes a sweep of one or more parameters using the
%           SIMENGINE compiler
%
%  Usage:
%      SIMSWEEP // runs the GUI
%      [OUT Y1 T1] = SIMSWEEP(MODEL, TIME [, OPTIONS ...])
%
%  Description:
%   SIMSWEEP creates a linear mapping between parameter/state
%   ranges and evaluates the intermediate points, running the
%   simulations in parallel.  Running without arguments invokes the
%   GUI interface.
%
%      MODEL is a full pathname to a DSL model file.
%
%      TIME is the simulation time limit. If scalar, it specifies a
%      simulation starting at T=0 proceeding to T=TIME. TIME must be
%      greater than zero. Otherwise, TIME may be a 2-element array
%      specifying a simulation starting at T=TIME(1) proceeding to
%      T=TIME(2). TIME(2) must be greater than TIME(1).
%
%      Additional optional parameters may follow:
%
%      'inputs'
%        Specifies the desired inputs as a structure of name-value
%        pairs.  Each value can either be a constant, a vector of
%        two values (signifying the beginning and end of a range),
%        or not specified to use the default value.
%
%      'states'
%        Specifies the initial state vector
%
%      'target'
%        Specifies the desired target (one of 'cpu',
%        'parallel-cpu', or 'gpu')
%
%      'precision'
%        Specifies the desired precision (one of 'single' or 'double')
%
%      'steps'
%        Specifies the number of steps to take in the linear sweep
%
%
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
% For more information, please visit http://www.simatratechnologies.com
%
function varargout = simsweep(dslfile, varargin)

if nargin == 0
  simsweep_gui
  return;
end

% Parse all the arguments
p = parseArgs(dslfile, varargin);

% Get model information
m = simex(dslfile);

% Create the input space
input_space = createInputSpace(m, p.Results);

% Create the output space
if length(p.Results.states) == 0
  state_space = m.default_states;
else
  state_space = p.Results.states;
end  

% Run the simulation
prec = ['-' p.Results.precision];
target = ['-' p.Results.target];
[o, final_states, tf] = simex(dslfile, p.Results.time, prec, target, input_space, state_space);

% Return output arguments
switch nargout
 case 3,
  varargout = {o, final_states, tf};
 case 2,
  varargout = {o, final_states};
 case {0,1},
  varargout = {o};
  varargout = {o};
 otherwise
  error('Simatra:SIMEX:outputArgumentError', ...
        'More than three output arguments were specified')
end  

end



%% parseArgs - parse all the arguments to the function
function p = parseArgs(dslfile, args)

p = inputParser; % Create an instance of the inputParser
p.addRequired('dslfile', @ischar);
p.addRequired('time', @(x)(isnumeric(x) && (length(x)==1 || (length(x)==2 ...
                                                  && x(1) < x(2)))));
p.addParamValue('target', 'parallel-cpu', @(x)any(strcmpi(x,{'cpu','parallel-cpu','gpu'})))
p.addParamValue('precision', 'single', @(x)any(strcmpi(x,{'single','float','double'})))
p.addParamValue('inputs', struct(), @isstruct)
p.addParamValue('states', [], @isnumeric)
p.addParamValue('steps', 100, @isnumeric)

p.parse(dslfile, args{:});

% Display all arguments.
%disp ' '
%disp 'List of all arguments:'
%disp(p.Results)

end

%% createInputSpace - creates a linear input space structure
function input_data = createInputSpace(m, args)

if length(args.time) == 1
  starttime = 0;
  stoptime = args.time;
else
  starttime = args.time(1);
  stoptime = args.time(2);
end
steps = args.steps;
inputs = m.input_names;
input_data = m.default_inputs;
fields = fieldnames(args.inputs);
for i=1:length(fields)
  if any(strcmpi(inputs, fields{i}))
    val = args.inputs.(fields{i});
    if length(val) == 2
      input_data.(fields{i}) = linspace(val(1), val(2), steps);
    else
      input_data.(fields{i}) = val;
    end
  else
    warning('Simatra:SIMSWEEP:argumentError', ['Invalid input with '...
                        'name ' fields{i}]);
  end
end

end