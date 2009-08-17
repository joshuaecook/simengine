%SIMEX   Executes a high-performance software simulation engine using the SIMENGINE compiler.
%
%   Usage:
%       M = SIMEX(MODEL)
%       [OUT Y1] = SIMEX(MODEL, TIME, INPUTS, Y0, ...)
%
%   Description:
%    SIMEX compiles the model defined in the DSL file into a
%    high-performance software simulation engine. SIMEX automatically
%    chooses the best available computing platform for the model and
%    executes the simulation with the given parameters. (I hope hope hope.)
%
%    SIMEX(MODEL, TIME, INPUTS, Y0, ...) accepts the following options, all
%    of which are optional except for the file name of the DSL model and
%    the simulation time limit.
%
%      MODEL is a full pathname to a DSL model file.
%
%      TIME is the simulation time limit. If scalar, it specifies a
%      simulation starting at T=0 proceeding to T=TIME. TIME must be
%      greater than zero. Otherwise, TIME may be a 2-element array
%      specifying a simulation starting at T=TIME(1) proceeding to
%      T=TIME(2). TIME(2) must be greater than TIME(1).
%
%      INPUTS is a structure containing model parameter values. The
%      field names of the structure correspond to model parameter names.
%      The associated values may be scalar or they may be arrays of
%      length N indicating that N parallel simulations are run. All
%      non-scalar values must have the same length. In parallel
%      simulations, all models receive the same value for scalar inputs.
%
%      Y0 is an array of model initial state values.
%
%      Additional optional parameters may follow:
%
%      '-outdir'
%        The next parameter must be a path which specifies the
%        directory in which the compiled model will be saved. (?Not
%        supported in Octave?)
%
%      '-double'
%        Constructs a simulation engine that computes in
%        double-precision floating point. (NOT CURRENTLY SUPPORTED
%        for CUDA mode.)
%
%      '-single'
%        Constructs a simulation engine that computes in
%        single-precision floating point.
%
%      '-mode'
%        The next parameter must be a mode identifier.
%        Currently only 'ode' and 'ode_cuda' are supported.
%        The default is 'ode_cuda.'
%
%      '-debug'
%        Enables the compiler to produce extra debugging
%        information.
%
%      '-profile'
%        Enables the compiler to produce extra profiling
%        information.
%
%    M = SIMEX(MODEL) compiles MODEL as above and returns a
%    model description structure M containing information
%    which describes the model states, parameters, and outputs.
%
%    Copyright 2009 Simatra Modeling Technologies, L.L.C.
%    For more information, please visit http://www.simatratechnologies.com
%
function [varargout] = simex(varargin)
[dslPath dslName modelFile opts] = get_simex_opts(varargin{:});

dllPath = invoke_compiler(dslPath, dslName, modelFile, opts);
interface = simex_helper(dllPath, '-query');

if nargin == 1
  varargout = {interface};
else
  userInputs = vet_user_inputs(interface, opts.inputs);
  userStates = vet_user_states(interface, opts.states);
  
  [inputsM inputsN] = size(userInputs);
  [statesM statesN] = size(userStates);
  models = max([1 inputsM statesM]);

  if 1 < inputsM && 1 < statesM && inputsM ~= statesM
    error('Simatra:SIMEX:argumentError', ...
          'When INPUTS and Y0 both contain more than 1 row, they must have the same number of rows.');
  end
  
  % User data are transposed before passing in to the simulation.
  if 0 == inputsM
    userInputs = zeros(interface.num_inputs, models);
    for i=[1:interface.num_inputs]
      userInputs(i,:) = interface.default_inputs.(interface.input_names(i)) * ones(1, models);
    end
  elseif 1 == inputsM && models ~= inputsM
    userInputs = transpose(userInputs) * ones(1, models);
  else
    userInputs = transpose(userInputs);
  end
  
  if 0 == statesM
    userStates = transpose(interface.default_states) * ones(1, models);
  elseif 1 == statesM && models ~= statesM
    userStates = transpose(userStates) * ones(1, models);
  else
    userStates = transpose(userStates);
  end
  
  [output y1] = simex_helper(dllPath, [opts.startTime opts.endTime], ...
                        userInputs, userStates);
  
  % Output data are transposed before returning.
  fnames = fieldnames(output);
  for i=[1:length(output)]
    for f=[1:length(fnames)]
      output(i).(fnames{f}) = ...
          transpose(output(i).(fnames{f}));
    end
  end
           
  varargout = {output transpose(y1)};
end
end
% 

function [dslPath dslName modelFile opts] = get_simex_opts(varargin)
%
% GET_SIMEX_OPTS parses the options from the command
% invocation.
%
% Returns the target output directory, the name of the DSL input
% model file (sans extension,) the path of the DSL input model
% file, and a struct containing command options.
%
dslPath = '';
dslName = '';
modelFile = '';
opts = struct('mode','', 'precision','double', ...
              'debug',false, 'profile',false, ...
              'startTime',0, 'endTime',0, ...
              'inputs',struct(), 'states',[], ...
              'simengine','');

[seroot] = fileparts(which('simex'));
opts.simengine = seroot;

if 1 > nargin
  help('simex')
  error('Simatra:SIMEX:argumentError', ...
        'SIMEX requires an input model file name.');
end

modelFile = varargin{1};
[pathName dslName] = fileparts(modelFile);

if 1 < nargin
  [opts.startTime opts.endTime] = get_time(varargin{2});

  for count=3:nargin
    arg = varargin{count};
    if isstruct(arg)
      opts.inputs = arg;
    elseif isnumeric(arg)
      opts.states = arg;
    elseif ~(ischar(arg) || isempty(arg))
      error('Simatra:SIMEX:argumentError', ...
            'All additional arguments must be non-empty strings.');
    elseif strcmpi(arg, '-double')
      opts.precision = 'double';
    elseif strcmpi(arg, '-single')
      opts.precision = 'float';
    elseif strcmpi(arg, '-debug')
      opts.debug = true;
    elseif strcmpi(arg, '-profile')
      opts.profile = true;
    elseif strcmpi(arg, '-mode')
      count = count + 1;
      if count > nargin
        error('Simatra:SIMEX:argumentError', ...
              'The -mode switch must be followed by a mode identifier.');
      end
      mode = lower(varargin{count});
      if ~(ismember(mode, ...
                    {'ode', 'ode_cuda'}))
        error('Simatra:SIMEX:argumentError', ...
              ['The specified mode ' mode ' is not available.']);
      end
      opts.mode = mode;
    end
  end
end

if isempty(dslPath)
  dslPath = pwd;
elseif 7 ~= exist(dslPath, 'dir')
  error('Simatra:SIMEX:fileNotFoundError', ...
        'The destination directory %s does not exist.', mexPath);
end

if ~exist(modelFile, 'file')
  error('Simatra:SIMEX:fileNotFoundError', ...
        'The input model file %s does not exist.', modelFile);
end

end
% 

function [startTime endTime] = get_time(userTime)
% GET_TIME returns a 2-element array containing the time limit for
% a simulation run.
data = double(userTime);

[rows cols] = size(userTime);

switch (rows * cols)
 case 1
  startTime = 0;
  endTime = userTime;
  if userTime < 0
    error('Simatra:SIMEX:argumentError', ...
          'TIME must be greater than zero.');
  end
 case 2
  startTime = userTime(1);
  endTime = userTime(2);
  if userTime(2) < userTime(1)
    error('Simatra:SIMEX:argumentError', ...
          'TIME(2) must be greater than TIME(1).');
  end
 otherwise
  error(Simatra:argumentError, 'TIME must have length of 1 or 2.');
end
end
% 

function [userInputs] = vet_user_inputs(interface, inputs)
% VET_USER_INPUTS verifies that the user-supplied inputs are valid.
% Returns a MxN matrix where N is the number of model inputs.
% M is the number of parallel models.
if ~isstruct(inputs)
  error('Simatra:typeError', ...
        'Expected INPUTS to be a structure.')
end

models = 0;

fieldnames = interface.input_names;
for fieldid=[1:length(fieldnames)]
  fieldname = fieldnames{fieldid};
  if ~isfield(inputs, fieldname)
    continue
  end

  field = inputs.(fieldname);
  
  if ~isnumeric(field)
    error('Simatra:typeError', 'Expected INPUTS.%s to be numeric.', fieldname);
  elseif issparse(field)
    error('Simatra:typeError', 'Did not expect INPUTS.%s to be sparse.', fieldname);
%  elseif iscomplex(field)
%    warning('Simatra:warning', 'Ignoring imaginary components of INPUTS.%s.', fieldname);
  end
  
  if ~isscalar(field)
    [rows cols] = size(field);
    if 2 < ndims(field)
      error('Simatra:valueError', 'INPUTS.%s may not have more than 2 dimensions.', fieldname);
    elseif ~(1 == rows || 1 == cols)
      error('Simatra:valueError', 'Expected INPUTS.%s to be a vector or scalar.', fieldname);
    end
    
    if 1 < models
      if models ~= length(field)
        error('Simatra:valueError', 'All non-scalar fields must have the same length.');
      end
    else
      models = max(rows, cols);
    end
  elseif 0 == models
    models = 1;
  end
end

userInputs = zeros(models, interface.num_inputs);
for fieldid=[1:length(fieldnames)]
  fieldname = fieldnames{fieldid};
  if ~isfield(inputs, fieldname)
    userInputs(1:models, fieldid) = interface.default_inputs.(fieldname) * ones(models, 1);
    continue
  end
  
  field = inputs.(fieldname);
  if isscalar(field)
    userInputs(1:models, fieldid) = double(field) * ones(models, 1);
  elseif length(field) == models
    userInputs(1:models, fieldid) = double(field);
  else
    error('Simatra:valueError', 'Expected INPUTS.%s to have length %d.', fieldname, models);
  end
end

end
% 

function [userStates] = vet_user_states(interface, states)
% VET_USER_STATES verifies that the user-supplied initial states
% contain valid data.
if ~isnumeric(states)
  error('Simatra:typeError', 'Expected Y0 to be numeric.');
elseif issparse(states)
  error('Simatra:typeError', 'Did not expect Y0 to be sparse.');
%elseif iscomplex(states)
%  warning('Simatra:warning', 'Ignoring imaginary components of Y0.');
end

[statesRows statesCols] = size(states);
userStates = [];

if 0 < statesRows && 0 < statesCols
  if statesCols ~= interface.num_states
    error('Simatra:SIMEX:argumentError', ...
          'Y0 must contain %d columns.', interface.num_states);
  end
  userStates = double(states);
end

end
% 

function [dllPath] = invoke_compiler(dslPath, dslName, modelFile, opts)
simengine = fullfile(opts.simengine, 'bin', 'simEngine');
setenv('SIMENGINE', opts.simengine);

status = simEngine_wrapper(simengine, modelFile, dslName);

if 0 ~= status
  error('Simatra:SIMEX:compileError', ...
        'Compilation returned status code %d.', status);
end

models = max(1, size(opts.states,1));
fnames = fieldnames(opts.inputs);
for fid = 1:size(fnames)
  models = max([models size(opts.inputs.(fnames{fid}))]);
end

if 1 == models
  target = 'CPU';
else
  target = 'GPU';
end

make = ['make MODEL=' dslName ...
        ' TARGET=' target ...
        ' SIMENGINE_STORAGE=' opts.precision ...
        ' NUM_MODELS=' num2str(models)];
status = system(make);

if 0 ~= status
  error('Simatra:SIMEX:compileError', ...
        'Make returned status code %d.', status);
end

% TODO what is the path of the resultant DLL?
dllPath = fullfile(pwd, 'libsimengine.so');

% TODO check the shape of the user inputs and start states, other
% parameters, and recompile the model if necessary.
end
% 

