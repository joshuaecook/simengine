% BUILDENGINE Creates a high-performance software simulation engine
% using the Diesel simulation compiler.
%
%    BUILDENGINE(MODEL) compiles the model defined in the DSL file
%    named MODEL using the high-performance software backend.
%    BUILDENGINE automatically creates two MEX-functions. One is 
%    a MEX-function having the the same name as the DSL model and 
%    is used for high-performance simulations.  This MEX-function
%    incorporates the numerical ODE solver in the engine.  The
%    second function has the same base name as the first function
%    with a _ode appended.  This function can be passed as a handle
%    to the existing Matlab ode solvers, such as ode15s, ode45, and
%    ode113.
%
%    M = BUILDENGINE(MODEL) compiles MODEL as above and returns a
%    model description structure M containing information
%    which describes the model states, inputs, and outputs.
%
%    Copyright 2009 Simatra Modeling Technologies, L.L.C.
%    For more information, please visit http://www.simatratechnologies.com
%
function [varargout] = buildEngine(varargin)

if nargin == 0
  error('Simatra:argumentError', ['buildEngine must be called with ' ...
                      'at least one input argument - the dsl filename ' ...
                      'to compile'])
  return
else
  filename = varargin{1};
end

[dynenv, returnStructure] = vetArgsAndEnvironment(filename, nargout);

mode = 'mex';
debug = false;
verbose = false;

[workingDirectory, basename] = fileparts(filename);
compileDirectory = '.';%[basename '.dso'];


if false %~(recompileNeeded(filename))
  disp('DSL file unmodified since last compile.');
  if returnStructure 
    s = getSimulationStructure(compileDirectory, sprintf('%s_struct', ...
                                                      basename));
    varargout = {s};
    disp(sprintf('Run "%s(time, s.inputs) to simulate ..."', basename))
    disp(sprintf(['or Run "[t, y] = ode45(@%s_ode, [t0 tf], [y0(1) y0(2) ' ...
                  '...])" to use the internal solvers ...'], basename));
  end
  return
end

% Clears any existing reference to the old model.
eval(sprintf('clear(''%s'')', basename))

%disp(['Compiling file ' filename]);

if true
%build a temporary file for diesel shell
%[newFilename, compileDirectory] = wrapDSL(filename, compileDirectory);

%check path for simEngine
simEngine = sprintf('%s/bin/simEngine', dynenv);
%run Diesel compiler on the new .dsl file, then remove file
%[status, result] = system(sprintf('%s %s', diesel, newFilename));
%delete(newFilename);
curdir = pwd;
cd(compileDirectory);
try
  status = simEngine_wrapper(simEngine, filename, basename);
catch me
  cd(curdir);
  me
  error('Simatra:CompileError', ['Can not invoke simEngine compiler. '...
                       'Please verify installation.'])
  return;
end
cd(curdir);

if status ~= 0
    error('Simatra:CompileError',['Status %d returned from Diesel Compiler '...
                             'process'], status);
%else
  %disp(result)
  %elementary error/warning parsing, this needs to be developed further
  %for now write the Diesel output to a log, this is to take care of
  %a problem where the results string was causing MATLAB errors
%  logFID = fopen('dieselOutput.log', 'w');
%  fprintf(logFID, '%s', result);
%  fclose(logFID);
end
end

s = getSimulationStructure(compileDirectory, sprintf('%s_struct', basename));
if returnStructure
  varargout = {s};
end



% Run the MEX commands
c_source = fullfile(compileDirectory, ...
                    sprintf('%s_%s.c',basename,mode));
ode_c_source = fullfile(compileDirectory, ...
                        sprintf('%s_ode%s.c',basename,mode));
solverlib = fullfile(dynenv, 'lib', ['solvers_' s.precision '.a']);
switch computer
 case 'GLNX86'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m32 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', basename, ['-I' fullfile(dynenv,'include')], c_source, solverlib);
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m32 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', [basename '_ode'], ['-I' fullfile(dynenv,'include')], ode_c_source);
 case 'GLNXA64'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m64 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', basename, ['-I' fullfile(dynenv,'include')], c_source, solverlib);
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m64 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', [basename '_ode'], ['-I' fullfile(dynenv,'include')], ode_c_source);
 case 'MACI'
  mex('-output', basename, ['-I' fullfile(dynenv,'include')], c_source, solverlib);
  mex('-output', [basename '_ode'], ['-I' fullfile(dynenv,'include')], ode_c_source);
 case 'MACI64'
  mex('-output', basename, ['-I' fullfile(dynenv,'include')], c_source, solverlib);
  mex('-output', [basename '_ode'], ['-I' fullfile(dynenv,'include')], ode_c_source);
 case 'i686-pc-linux-gnu'
  mex('--output', [basename '.mex'], ['-I' fullfile(dynenv,'include')], c_source, solverlib);
  mex('--output', [basename '_ode.mex'], ['-I' fullfile(dynenv,'include')], ode_c_source);  
 otherwise
  error('Simatra:PlatformError', 'Unsupported platform');
end


% Copy the help file if it exists
matlab_help = sprintf('%s/%s_%s_help.m',compileDirectory, basename, mode);
if exist(matlab_help,'file')
  % copy file to current directory
  copyfile(matlab_help,sprintf('%s.m',basename));
  % help(basename)
end

disp(sprintf('Run "%s(time, s.inputs) to simulate ..."', basename));
disp(sprintf(['Or run "[t, y] = ode45(@%s_ode, [t0 tf], [y0(1) y0(2) ' ...
              '...])" to execute'], basename));

end
% 

% Indicates whether the model represented by the DSL file ``filename''
% needs to be recompiled.
function [recompile] = recompileNeeded(filename)
[path, basename] = fileparts(filename);

DSLFileData = dir(filename);

recompile1 = true;
recompile2 = true;
compiled_file = fullfile('.', [basename '.' mexext]);
ode_compiled_file = fullfile('.', [basename '_ode.' mexext]);

if exist(compiled_file, 'file')
  MEXFileData = dir(compiled_file);
  timedelta = MEXFileData.datenum - DSLFileData.datenum;
  recompile1 = timedelta < 0;
end

if exist(ode_compiled_file, 'file')
  MEXFileData = dir(ode_compiled_file);
  timedelta = MEXFileData.datenum - DSLFileData.datenum;
  recompile2 = timedelta < 0;
end

recompile = recompile1 || recompile2;

end
% 

% Takes a DSL model definition file as input and creates appropriate
% wrappers for the DIESEL compiler.  Returns the name of the temporary 
% DSL file and the compile directory.
%                                              
function [newFilename, compileDirectory] = wrapDSL(filename, compileDirectory)
[workingDirectory, basename] = fileparts(filename);
if isempty(workingDirectory)
  workingDirectory = cd
end
oldFileHandle = fopen(filename, 'r');
newFilename = tempname(workingDirectory);
newFileHandle = fopen(newFilename, 'w');

% Settings for the new file to run the MATLAB compiler
fprintf(newFileHandle, '%s\n', ...
        'settings.compiler.matlabmode.setValue(true)');

fprintf(newFileHandle, '%s\n', ...
        ['settings.compiler.targetlocation' ...
         '.setValue("' compileDirectory '")']);

% Reads in the original DSL code.
% FIXME: textscan function missing in OCTAVE
dslCode = textscan(oldFileHandle, '%s', 'Delimiter', '\n');
dslCode = dslCode{1};
% Parses the DSL code to extract the model name,
% assuming the last appearance of the ``model'' keyword
% indicates the top-level model.
modelName = '';
for i = 1:length(dslCode)
  match = regexp(dslCode(i), '^model\s+(\(\w+(\s*,\s*\w+)*\)\s*=\s*)?(?<model>\w+)', 'names');
  if ~(isempty(match{1}))
    % Some weirdness here due to how MATLAB wraps results of regexp in
    % cell array.  We're assuming that the last model definition is the
    % top-level mode.
    modelName = match{1}.model;
  end
  
  % Writes the old file model into the new model.
  fprintf(newFileHandle, '%s\n', dslCode{i});
end
if isempty(modelName)
  error('Simatra:CompileError', 'Unable to determine top-level model name')
end
disp(['Using ' modelName ' as top-level model.']);

fprintf(newFileHandle, '%s\n', ['model ' modelName ...
                    '_matlabInstance = ' modelName '.new()']);
fprintf(newFileHandle, '%s\n', ['compile(' modelName ...
                    '_matlabInstance)']);

fclose(newFileHandle);
fclose(oldFileHandle);
end
% 

function [dynenv,returnStructure] = vetArgsAndEnvironment(filename, nargout)
% Tests for dynamo environmental variable.
% find the location of buildEngine
buildengineloc = which('buildEngine');
% the install location is that directory
[dynenv] = fileparts(buildengineloc);

% need to set it for simengine
setenv('SIMENGINE', dynenv);

%if exist(getenv('SIMENGINE'),'dir')
%  dynenv = getenv('SIMENGINE');
%else
%  error('Simatra:CompileError','SIMENGINE environment variable not set to install location');
%end

% Ensures the DSL source file exists.
if ~(exist(filename,'file'))
  error('Simatra:ArgumentError','No such file ''%s'' available', filename);
end

% Checks for correct number of arguments.
if nargout > 1
  error('Simatra:ArgumentError','Only one optional output quantity is allowed');
else
  % Return the structure only if there is an output container specified
  returnStructure = (nargout == 1);
end

end
% 

% Adds the compile directory to the path, 
% loads in the structure contained in the file ``model.m'', 
% then removes that directory from the path.
% The file ``model.m'' defines the dif array/structure.
%
function s = getSimulationStructure(compileDirectory, funname)

s = [];

if not(exist(sprintf('%s/%s.m',compileDirectory,funname),'file')) 
  error('Simatra:dataStructError', ['No input data structure found ' ...
                      'at location %s/%s.m'], compileDirectory, ...
                                                funname)
end

addpath(compileDirectory)
clear(funname)
eval(funname)
rmpath(compileDirectory)

% Puts model structure data from ``model.m'' into return structure
% state_names = {};
% state_values = [];
% param_names = {};
% param_values = [];
% inputs = {};
% outputs = {};
% if isfield(dif, 'states')
%   n = length(dif.states);
%   state_names = cell(1,n);
%   state_values = zeros(1,n);
%   for i=1:n
%     state_names{i} = dif.states(i).name;
%     state_values(i) = dif.states(i).init;
%   end
% end
% if isfield(dif, 'extpar')  
%   n = length(dif.extpar);
%   param_names = cell(1,n);
%   param_values = zeros(1,n);
%   for i=1:n
%     param_names{i} = dif.extpar(i).name;
%     param_values(i) = dif.extpar(i).value;
%   end
% end
% if isfield(dif, 'inputs')
%   n = length(dif.inputs);
%   inputs = cell(1,n);
%   for i=1:n
%     inputs{i} = dif.inputs(i).name;
%   end
% end
% if isfield(dif, 'outputs')
%   n = length(dif.outputs);
%   outputs = cell(1,n);
%   for i=1:n
%     outputs{i} = dif.outputs(i).name;
%   end
% end
s = struct();
s.name = funname(1:end-7);
%s.state_names = state_names;
%s.state_values = state_values;
%s.param_names = param_names;
%s.param_values = param_values;
s.inputs = dif.inputs;
num_states = length(dif.states);
s.state_names = cell(1,num_states);
s.state_inits = zeros(1,num_states);
s.precision = dif.precision;
for i=1:num_states,
  s.state_names{i} = dif.states(i).name;
  s.state_inits(i) = dif.states(i).init;
end
%s.outputs = outputs;
end