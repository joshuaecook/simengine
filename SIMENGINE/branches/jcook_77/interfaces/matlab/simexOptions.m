function [options] = simexOptions (dsl, varargin)
  % SIMEXOPTIONS Parses the arguments from command invocation.
  options = struct('simengine','', 'model', '', 'instances',1, 'startTime',0, ...
                   'stopTime',0, 'inputs',struct(), 'states',[], ...
                   'outputs', '', 'debug', false, 'args', '--binary', ...
                   'dslfile', '', 'target', 'parallelcpu', 'precision', ...
                   'double');
  userOptions = varargin(:);

  [seroot] = fileparts(which('simex'));
  options.simengine = fullfile(seroot, 'bin', 'simEngine');

  if ~exist(dsl, 'file')
      simexError('argumentError', ...
                 ['DSL model file ' dsl ' does not exist. Please ' ...
                  'specify a DSL model filename as the first ' ...
                  'argument.']);
  else
      fid = fopen(dsl, 'r');
      if fid < 0
          simexError('argumentError', ...
                     ['DSL model file ' dsl ' cannot be opened for ' ...
                      'reading. Please check the file ' ...
                      'permissions.']);
      else
          fclose(fid);
      end      
  end
  options.dslfile = dsl;
  options.model = fullfile(dsl);
  
  % Create a new temporary data path
  options.outputs = ['.simex' num2str(now,'%16f')];

  if 0 < length(userOptions)
      if isnumeric(userOptions{1})
          [options.startTime options.stopTime] = getTime(userOptions{1});
          userOptions = userOptions(2:length(userOptions));
      end
  end
  
  if 0 < length(userOptions)
      if isstruct(userOptions{1})
          options.inputs = userOptions{1};
          userOptions = userOptions(2:length(userOptions));
      end
  end
      
  while ~isempty(userOptions)
      [options, userOptions] = getOption(options, userOptions);
  end

  instances = max(size(options.inputs));
  if 1 == instances
      % Determine the number of instances by looking for cell
      % arrays within the input structure
      names = fieldnames(options.inputs);
      for inputid = 1:size(names)
          value = options.inputs.(names{inputid});

          if iscell(value)
              [rows cols] = size(value);
              if 2 < ndims(value)
                  simexError('valueError', ['INPUTS.' fieldname ' may not have more than 2 dimensions.']);
              elseif 1 ~= min([rows cols])
                  simexError('valueError', ['INPUTS.' field ' must have one scalar dimension.']);
              end
              if 1 == instances
                  instances = max([rows cols]);
              elseif instances ~= max([rows cols])
                  simexError('valueError', 'All cell array INPUT fields must have the same length.');
              end
          elseif ~isscalar(value)
              simexError('valueError', ['INPUTS.' field ' must be a scalar or cell array.']);
          end
      end
  else
      simexError('argumentError', 'Unexpected dimensions of INPUTS.');
  end

  options.instances = instances;

  instances = max([1 size(options.states,1)]);
  if 0 ~= instances && 1 ~= instances && options.instances ~= instances
      simexError('argumentError', ...
                 ['Y0 must contain 1 or ' options.instances ' rows.']);
  end
  
  options.instances = max([instances options.instances]);
end


%%
function [startTime stopTime] = getTime(userTime)
  % GETTIME Returns a 2-element array containing the time limit for a simulation run.
  [rows cols] = size(userTime);

  switch (rows * cols)
    case 1
      startTime = 0;
      stopTime = double(userTime);
      if userTime < 0
          simexError('argumentError', ...
                     'TIME must be greater than zero.');
      end
    case 2
      startTime = double(userTime(1));
      stopTime = double(userTime(2));
      if stopTime < startTime
          simexError('argumentError', ...
                     'TIME(2) must be greater than TIME(1).');
      end
    otherwise
      simexError('argumentError', 'TIME must have length of 1 or 2.');
  end
end



%%
function [options, restUserOptions] = getOption(options, userOptions)
  opt = userOptions{1};
  restUserOptions = userOptions(2:length(userOptions));
  
  if ~(ischar(opt) && ~isempty(opt))
      simexError('argumentError', ...
                 ['Additional options to SIMEX must be non-empty strings.']);
  end
  if '-' ~= opt(1) || 1 == length(opt)
      simexError('argumentError', ...
                 ['Invalid or unrecognized option ' opt '.']);
  end
  
  switch opt(2:length(opt))
    case 'float'
      options.precision = 'float';
      options.args = [options.args ' --precision float'];
    case 'cpu'
      options.target = 'cpu';
      options.args = [options.args ' --target cpu'];
    case 'parallelcpu'
      options.target = 'parallelcpu';
      options.args = [options.args ' --target parallelcpu'];
    case 'gpu'
      options.target = 'gpu';
      options.args = [options.args ' --target gpu'];
    case 'resume'
      options.resume = true;
      if ~isnumeric(userOptions{2})
          simexError('argumentError', ...
                     'Y0 must be numeric');
      end
      options.states = userOptions{2};
      restUserOptions = userOptions(3:length(userOptions));

    % Undocumented options follow
    case 'debug'
      options.debug = true;
      options.args = [options.args ' --debug'];
    case 'profile'
      options.profile = true;
      options.args = [options.args ' --profile'];
    
    % Any other options are passed to simEngine
    otherwise
      if 2 == length(opt)
          options.args = [options.args ' ' opt];
      else
          options.args = [options.args ' -' opt];
      end
      
      if ~isempty(restUserOptions)
          optArg = getOptionArgument(userOptions{2});
          if optArg
              options.args = [options.args ' ' optArg];
              restUserOptions = userOptions(3:length(userOptions));
          end
      end
  end
end

%%
function [arg] = getOptionArgument(arg)
  % GETOPTIONARGUMENT Returns a string representation of the given
  % value if it is valid for an option argument. Returns false if
  % the value is not valid.
  if isnumeric(arg)
      if isscalar(arg)
          arg = num2str(arg);
      else
          simexError('argumentError', ...
                     ['Nonscalar numeric arguments are not supported.']);
      end
  elseif ~ischar(arg)
      simexError('argumentError', ...
                 ['Additional options to SIMEX must be non-empty strings.']);
  elseif '-' == arg(1)
      arg = false;
  end
end
