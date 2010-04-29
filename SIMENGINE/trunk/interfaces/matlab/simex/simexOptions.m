function [options] = simexOptions (dsl, varargin)
  % SIMEXOPTIONS Parses the arguments from command invocation.
  options = struct('simengine','', 'model', '', 'instances',1, 'startTime',0, ...
                   'stopTime',0, 'inputs',struct(), 'states',[], ...
                   'outputs', '', 'jsonfile', '', 'debug', false, 'args', '--binary', ...
                   'dslfile', '', 'target', 'default', 'precision', ...
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

  % Set up path to JSON interface file and add commandline argument
  options.jsonfile = fullfile(options.outputs, 'simex_interface.json');
  options.args = [options.args ' --json_interface ' options.jsonfile];

  if 0 < length(userOptions)
    if isnumeric(userOptions{1})
      [options.startTime options.stopTime] = getTime(userOptions{1});
      userOptions = userOptions(2:length(userOptions));
      % Corresponding command-line arguments for time
      if options.stopTime ~= 0
        options.args = [options.args ' --start ' num2str(options.startTime)];
        options.args = [options.args ' --stop ' num2str(options.stopTime)];
      end
    end
  end
  
  if 0 < length(userOptions)
    if isstruct(userOptions{1})
      options.inputs = userOptions{1};
      userOptions = userOptions(2:length(userOptions));

      % Add command-line option to tell simulation which inputs were
      % user-specified
      names = fieldnames(options.inputs);
      if length(names) > 0
        nameslist = names{1};
        for n = 2:length(names)
          nameslist = [nameslist ':' names{n}];
        end
        options.args = [options.args [' --inputs ' nameslist]];
      end
    end
  end
      
  while ~isempty(userOptions)
      [options, userOptions] = getOption(options, userOptions);
  end

  % add target and binary options
  switch options.target
   case 'cpu'
    options.args = [options.args ' --target cpu'];
   case 'parallelcpu'
    options.args = [options.args ' --target parallelcpu'];
   case 'gpu'
    options.args = [options.args ' --target gpu'];
   otherwise
    options.args = [options.args ' --target default'];
  end
    
  instances = max(size(options.inputs));
  if 1 == instances
      % Determine the number of instances by looking for cell
      % arrays within the input structure
      names = fieldnames(options.inputs);
      for inputid = 1:size(names)
          field = names{inputid};
          value = options.inputs.(field);
	  

          if iscell(value)
              [rows cols] = size(value);
              if 2 < ndims(value)
                  simexError('valueError', ['INPUTS.' field ' may not have more than 2 dimensions.']);
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

  if 0 ~= instances && 1 ~= instances && 1 ~= options.instances && options.instances ~= instances
    simexError('argumentError', ['INPUTS must have 1 or ' num2str(options.instances) ' values because -instances was specified.'])
  end

  options.instances = max([instances options.instances]);

  instances = max([1 size(options.states,1)]);
  if 0 ~= instances && 1 ~= instances && options.instances ~= 1 && options.instances ~= instances
      simexError('argumentError', ...
                 ['States passed to -resume must contain 1 or ' num2str(options.instances) ' rows to match inputs or -instances option.']);
  end
  
  options.instances = max([instances options.instances]);
  options.args = [options.args ' --instances ' num2str(options.instances)];
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
  restUserOptions = userOptions(2:end);
  
  if (~ischar(opt) && ~isempty(opt))
      opt
      simexError('argumentError', ...
                 ['Additional options to SIMEX must be non-empty strings.']);
  end
  if '-' ~= opt(1) || 1 == length(opt)
      simexError('argumentError', ...
                 ['Invalid or unrecognized option ' opt '.']);
  end
  
  switch opt(2:end)
    case 'double'
      options.precision = 'double';
      options.args = [options.args ' --precision double'];
    case {'float','single'}
      options.precision = 'float';
      options.args = [options.args ' --precision float'];
    case 'gpu'
      options.target = 'gpu';
    case 'cpu'
      options.target = 'cpu';
    case 'parallelcpu'
      options.target = 'parallelcpu';
    case 'instances'
      if length(userOptions) < 2 || ~isscalar(userOptions{2}) || floor(userOptions{2}) ~= userOptions{2} || userOptions{2} < 0
          simexError('argumentError', 'A positive integer value must be passed to -instances.');
      end
      options.instances = userOptions{2};
      restUserOptions = userOptions(3:end);
    case 'resume'
      options.resume = true;
      if length(userOptions) < 2 || ~isnumeric(userOptions{2})
          simexError('argumentError', ...
                     'Missing or invalid states passed to -resume.');
      end
      options.states = userOptions{2};
      restUserOptions = userOptions(3:end);
      
    % Deprecated(?) options
    case 'single'
      options.precision = 'float';
      options.args = [options.args ' --precision float'];
    case 'double'
      options.precision = 'double';
      options.args = [options.args ' --precision double'];
    case 'cpu'
      options.target = 'cpu';
      options.args = [options.args ' --target cpu'];
    case 'parallelcpu'
      options.target = 'parallelcpu';
      options.args = [options.args ' --target parallelcpu'];


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
              restUserOptions = userOptions(3:end);
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
