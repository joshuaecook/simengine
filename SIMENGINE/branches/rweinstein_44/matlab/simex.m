%SIMEX   Executes a high-performance software simulation engine using the SIMENGINE compiler.
%
%   Usage:
%       M = SIMEX(MODEL)
%       [OUT Y1 T1] = SIMEX(MODEL, TIME, INPUTS, Y0, ...)
%       [T Y] = SIMEX(MODEL, TIME, INPUTS, Y0, '-solver=MATLABODE')
%
%   Description:
%    SIMEX compiles the model defined in the DSL file into a
%    high-performance software simulation engine. SIMEX generates a
%    specially tuned executable version of the model for the selected computing
%    platform and executes the simulation with the given parameters.
%
%    SIMEX(MODEL, TIME, INPUTS, Y0, ...) accepts the following options, all
%    of which are optional except for the file name of the DSL model and the
%    time to execute the simulation.
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
%        directory in which the compiled model will be saved.
%
%      '-double'
%        Constructs a simulation engine that computes in
%        double-precision floating point. (This is the default.)
%
%      '-single' '-float'
%        Constructs a simulation engine that computes in
%        single-precision floating point.
%
%      '-cpu'
%        Constructs a serialized cpu-based simulation engine.
%        (This is the default.)
%
%      '-parallel-cpu'
%        Constructs a multiprocessor cpu-based simulation engine.
%
%      '-gpu'
%        Constructs a massively parallel gpu-based simulation
%        engine for CUDA-compatible Nvidia devices.
%
%      '-debug'
%        Enables the compiler to produce extra debugging
%        information.
%
%      '-dontrecompile'
%        Tells the compiler to not recompile the model.
%
%      '-profile'
%        Enables the compiler to produce extra profiling
%        information.
%
%      '-emulate'
%        Constructs a simulation engine that executes in GPU
%        emulation. (ONLY VALID FOR GPU TARGET.)
%
%      '-solver=MATLABODE'
%        Utilize a particular solver builtin to MATLAB, such as
%        ode15s, ode23t, and ode45.
%
%    M = SIMEX(MODEL) compiles MODEL as above and returns a
%    model description structure M containing information
%    which describes the model states, parameters, and outputs.
%
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
% For more information, please visit http://www.simatratechnologies.com
%
function [varargout] = simex(varargin)

if nargin == 0
  simex_gui
  return;
end

[dslPath dslName modelFile opts] = get_simex_opts(varargin{:});

if exist(modelFile, 'file')
  dllPath = invoke_compiler(dslPath, dslName, modelFile, opts);
  interface = simex_helper(dllPath, '-query');
else
  error('Simatra:SIMEX:rgumentError', ['File ''' modelFile ''' does ' ...
                      'not exist'])
end

if nargin == 1
  varargout = {interface};
else
  userInputs = vet_user_inputs(interface, opts.inputs);
  userStates = vet_user_states(interface, opts.states);
  
  inputsM = size(userInputs,1);
  statesM = size(userStates,1);
  models = max([1 inputsM statesM]);

  if 1 < inputsM && 1 < statesM && inputsM ~= statesM
    error('Simatra:SIMEX:argumentError', ...
          'When INPUTS and Y0 both contain more than 1 row, they must have the same number of rows.');
  end
  
  % User data are transposed before passing in to the simulation.
  if 0 == inputsM
    userInputs = zeros(interface.num_inputs, models);
    for i=1:interface.num_inputs
      userInputs(i,:) = interface.default_inputs.(interface.input_names{i}) * ones(1, models);
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
  
  tic;
    if opts.solver % use a MATLAB ODE solver
      simex_helper(dllPath, userInputs); % Initialize evalflow DLL
      try
        [t, y] = feval(opts.solver, @simex_helper, [opts.startTime opts.endTime], ...
                       userStates);
      catch me
        me
      end
      simex_helper(dllPath, userInputs); % Cleanup evalflow DLL
      varargout = {t, y};
      elapsed = toc;
      disp([dslName ' simulation using ' opts.solver ' completed in ' num2str(elapsed) ' ' ...
                          'seconds.']);
      return;
    else
      [output y1 t1] = simex_helper(dllPath, [opts.startTime opts.endTime], ...
                                    userInputs, userStates);
    end
  elapsed = toc;
  disp([dslName ' simulation completed in ' num2str(elapsed) ' seconds.']);
  

  % Output data are transposed before returning.
  fnames = fieldnames(output);
  for i=1:length(output)
    for f=1:length(fnames)
      output(i).(fnames{f}) = ...
          transpose(output(i).(fnames{f}));
    end
  end
           
  varargout = {output transpose(y1) t1};
end
end

%% 
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
opts = struct('models',1, 'target','', 'precision','double', ...
              'debug',false, 'profile',false, 'emulate',false, ...
              'recompile',true,'startTime',0, 'endTime',0, ...
              'inputs',struct(), 'states',[], ...
              'simengine','', 'solver', []);

[seroot] = fileparts(which('simex'));
opts.simengine = seroot;

if 1 > nargin
  help('simex')
  error('Simatra:SIMEX:argumentError', ...
        'SIMEX requires an input model file name.');
end

modelFile = realpath(varargin{1});
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
    elseif strcmpi(arg, '-float')
      opts.precision = 'float';
    elseif strcmpi(arg, '-cpu')
      opts.target = 'CPU';
    elseif strcmpi(arg, '-gpu')
      opts.target = 'GPU';
    elseif strcmpi(arg, '-parallel-cpu')
      opts.target = 'PARALLELCPU';
    elseif strcmpi(arg, '-debug')
      opts.debug = true;
    elseif strcmpi(arg, '-emulate')
      opts.emulate = true;
    elseif strcmpi(arg, '-profile')
      opts.profile = true;
    elseif strcmpi(arg, '-dontrecompile')
      opts.recompile = false;
    elseif length(arg) > 8 && strcmpi(arg(1:8), '-solver=')
      opts.solver = arg(9:end);
      if not(exist(opts.solver)) 
        error('Simatra:SIMEX:argumentError', ...
              '%s is not a valid matlab ode solver', opts.solver);
      end
    end
  end
  
  models = max(1, size(opts.states,1));
  fnames = fieldnames(opts.inputs);
  for fid = 1:size(fnames)
    models = max([models size(opts.inputs.(fnames{fid}))]);
  end
  opts.models = models;
end

if strcmpi(opts.target, '')
  opts.target = 'CPU';
  if 1 < opts.models
    switch computer
     case {'MACI','MACI64','i386-apple-darwin9.8.0','i386-apple-darwin8.9.1'}
      opts.target = 'PARALLELCPU';
     otherwise
      opts.target = 'PARALLELCPU';
      %      opts.target = 'GPU';
    end
  end
end

if isempty(dslPath)
  dslPath = pwd;
elseif 7 ~= exist(dslPath, 'dir')
  error('Simatra:SIMEX:fileNotFoundError', ...
        'The destination directory %s does not exist.', mexPath);
end

% if 2 ~=  exist(modelFile, 'file')
%   error('Simatra:SIMEX:fileNotFoundError', ...
%         'The input model file %s does not exist.', modelFile);
% end

end

%% 
function [startTime endTime] = get_time(userTime)
% GET_TIME returns a 2-element array containing the time limit for
% a simulation run.
[rows cols] = size(userTime);

switch (rows * cols)
 case 1
  startTime = 0;
  endTime = double(userTime);
  if userTime < 0
    error('Simatra:SIMEX:argumentError', ...
          'TIME must be greater than zero.');
  end
 case 2
  startTime = double(userTime(1));
  endTime = double(userTime(2));
  if endTime < startTime
    error('Simatra:SIMEX:argumentError', ...
          'TIME(2) must be greater than TIME(1).');
  end
 otherwise
  error('Simatra:argumentError', 'TIME must have length of 1 or 2.');
end
end

%% 
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
for fieldid=1:length(fieldnames)
  fieldname = fieldnames{fieldid};
  if ~isfield(inputs, fieldname)
    if isnan(interface.default_inputs.(fieldname))
      error('Simatra:valueError', 'INPUTS.%s has no default value and must be specified.', fieldname);
    end
    continue
  end

  field = inputs.(fieldname);
  
  if ~isnumeric(field)
    error('Simatra:typeError', 'Expected INPUTS.%s to be numeric.', fieldname);
  elseif issparse(field)
    error('Simatra:typeError', 'Did not expect INPUTS.%s to be sparse.', fieldname);
%  elseif iscomplex(field)
%    warning('Simatra:warning', 'Ignoring imaginary components of INPUTS.%s.', fieldname);
  elseif any(isnan(field))
    error('Simatra:valueError', 'INPUTS.%s may not contain NaN values.', fieldname);
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
for fieldid=1:length(fieldnames)
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

%% 
function [userStates] = vet_user_states(interface, states)
% VET_USER_STATES verifies that the user-supplied initial states
% contain valid data.
if ~isnumeric(states)
  error('Simatra:typeError', 'Expected Y0 to be numeric.');
elseif issparse(states)
  error('Simatra:typeError', 'Did not expect Y0 to be sparse.');
%elseif iscomplex(states)
%  warning('Simatra:warning', 'Ignoring imaginary components of Y0.');
elseif any(isnan(states))
  error('Simatra:valueError', 'Y0 may not contain NaN values.');
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

%% 
function [dllPath] = invoke_compiler(dslPath, dslName, modelFile, opts)
simengine = fullfile(opts.simengine, 'bin', 'simEngine');
setenv('SIMENGINE', opts.simengine);

%disp(['simEngine: ' simengine ', modelFile: ' modelFile ', dslName: ' ...
%       dslName])

if opts.recompile
  tic;
    status = simEngine_wrapper(simengine, modelFile, dslName);
  elapsed = toc;
  disp(['simEngine compiler completed in ' num2str(elapsed) ' seconds.']);

  if 0 ~= status
      error('Simatra:SIMEX:compileError', ...
            'Compilation returned status code %d.', status);
  end
  
  target = opts.target;
  if strcmp(opts.target, 'PARALLELCPU')
    target = 'OPENMP';
  end

  % determine architecture
  switch computer
   case {'PCWIN', 'GLNX86', 'MACI'}
    arch = 'i386';
   case {'PCWIN64', 'GLNXA64', 'SOL64', 'MACI64'}
    arch = 'x86_64';
   otherwise
    warning('Simatra:simEngine:simex', ['Architecture is not officially '...
            'supported'])
  end  

  
  make = ['make remake' ...
        ' -f' fullfile(opts.simengine, 'share/simEngine/Makefile') ...
        ' SIMENGINEDIR=' opts.simengine ...
        ' MODEL=' modelFile ...
        ' TARGET=' target ...
        ' ARCH=' arch ...
        ' SIMENGINE_STORAGE=' opts.precision ...
        ' NUM_MODELS=' num2str(opts.models) ...
        ' &> simex_make.log'];

if opts.debug
  make = [make ' DEBUG=1'];
end
if opts.emulate
  make = [make ' EMULATE=1'];
end
if opts.profile
  make = [make ' PROFILE=1'];
end

tic;
status = system(make);
elapsed = toc;

if 0 ~= status
  error('Simatra:SIMEX:compileError', ...
        'Make returned status code %d.', status);
end
disp([opts.target ' compiler completed in ' num2str(elapsed) ' seconds.']);
end % end if recompile

% TODO what is the path of the resultant DLL?
switch computer
  case {'MACI', 'MACI64','i386-apple-darwin9.8.0','i386-apple-darwin8.9.1'}
   dllPath = fullfile(pwd, 'libsimengine.dylib');
 otherwise
   dllPath = fullfile(pwd, 'libsimengine.so');
end  

% TODO check the shape of the user inputs and start states, other
% parameters, and recompile the model if necessary.
end

%%
function [abspath] = realpath(relpath, root)
% REALPATH returns a fully-qualified absolute path for a given
% relative path. The ROOT parameter is optional. If given, RELPATH
% is taken as relative to ROOT. If omitted, RELPATH is treated as
% relative to the current working directory.
[dir file ext ver] = fileparts(relpath);
if isempty(dir)
    dir = '.';
end 
command = ['cd ' dir ';'...
           ' echo $(pwd)/' file ext ver ';'];
if nargin > 1
  command = ['cd ' root '; ' command];
end
[status, abspath] = system(command);
abspath = strtrim(abspath);
end

