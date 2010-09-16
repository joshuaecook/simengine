function [outputs y1 t1 interface] = simEngine (options)
%  SIMENGINE compiles and/or executes a simulation
  global SIMEX_TIMEOUT;

  writeUserStates(options);
  writeUserInputs(options);

  command = ['"' options.simengine '" --inferior-mode --simex "' options.dslfile ...
             '" --outputdir "' options.outputs '" ' options.args];
  workingDir = options.outputs;

  if options.debug
    disp(['Running <' command '>'])
  end

  % File names
  logFile = fullfile(workingDir, 'logfile');
  compilationProgressFile = fullfile(workingDir, 'compilation_progress');
  simulationProgressFile = fullfile(workingDir, 'simulation_progress');
  statusFile = fullfile(workingDir, 'status');
  pidFile = fullfile(workingDir, 'pid');

  command = ['(' command ' &> "' logFile '" & pid=$! ; echo $pid > "' pidFile '" ; wait $pid; echo $? > "' statusFile '")&'];
  [stat, ignore] = system(command);
  while ~exist(pidFile,'file') || isempty(fileread(pidFile))
    pause(0.1);
  end
  % Ignore the newline
  pid = str2num(fileread(pidFile));
  % Remove the file to prevent crosstalk across launchBackground calls
  delete(pidFile);

  c = onCleanup(@()cleanupBackgroundProcess(pid));

  start = tic;
  outputlen = 0;
  messagelen = 0;
  log = '';

  while(processRunning(pid))
    % Map progress file to memory (only happens once, but inside
    % loop as we don't know when progress file will be created)
    if(~exist('m','var') && exist(simulationProgressFile, 'file'))
      m = memmapfile(simulationProgressFile, 'format', 'double');
      % Handle race condition where file is created but data
      % not yet written
      try
        m.Data;
      catch it
        clear m;
      end
    end

    % Update status bar
    if(exist('m','var'))
      progress = 100*sum(m.Data)/length(m.Data);
      message = sprintf('Simulating : %0.2f %%', progress);
      messagelen = statusBar(message, messagelen);
    else
      try
        message = fileread(compilationProgressFile);
	messagelen = statusBar(message, messagelen);
      catch it
	% This just updates the status bar, don't throw an error
	% if something goes wrong
      end
    end

    % Read interface (only happens once, but inside loop as we don't know 
    % when interface file will be created)
    if(~exist('interface', 'var') && exist(options.jsonfile, 'file'))
       interface = parseInterface(options);
    end

    % Initializes output return structures on first call.
    % Allows mex to perform reallocs if necessary, acts as just a pause otherwise
    if(exist('interface', 'var'))
      readSimulationData(interface, options);
    end

    % Update compiler output to Matlab console
    if exist(logFile, 'file')
      try
        log = fileread(logFile);
      catch it
        simFailure('launchBackground', 'Unable to read from simEngine log file.')
      end
    end
    if ~options.quiet && length(log) > outputlen
      fprintf('%s', log(outputlen+1:end));
      outputlen = length(log);
    end

    % Check to see if there is a timeout enabled
    if(~isempty(SIMEX_TIMEOUT) && toc(start) > SIMEX_TIMEOUT)
      simEngineError('SIMEX_TIMEOUT', ['Simulation did not complete within ' num2str(SIMEX_TIMEOUT) ' seconds.'])
    end
  end

  % Print any remaining compiler output to Matlab console
  try
    log = fileread(logFile);
  catch it
    simFailure('launchBackground', 'Process log file does not exist.')
  end
  if ~options.quiet && length(log) > outputlen
    fprintf('%s', log(outputlen+1:end));
  end

  % Check the return status of simEngine
  if exist(statusFile, 'file')
    status = str2num(fileread(statusFile));
    % Prevent any crosstalk between launchBackground calls
    delete(statusFile);
    if(exist(compilationProgressFile,'file'))
      messagelen = statusBar('', messagelen);
      delete(compilationProgressFile);
    end
    if(exist(simulationProgressFile,'file'))
      messagelen = statusBar('', messagelen);
      delete(simulationProgressFile);
    end
    delete(logFile);        
  else
    % Process is still running, not yet produced status (probably CTRL+C)
    status = -1;    
  end

  % Report error conditions to user
  if(128 == status)
    simEngineError('simCompile', ['Model ' options.dslfile ' can not be '...
				  'compiled']);
  elseif (129 == status)
    disp(['Please execute ''help simex'' for information on using '...
          'simEngine in MATLAB.'])
    simEngineError('simCompile', ['Can not execute simEngine due to invalid command line options'])
  elseif(-1 == status)
    simEngineError('simCompile', ['User terminated simulation.'])
  elseif (156 == status)
      % This does not seem like an issue at all - it happens enough with no
      % side effects. Just ignore for now. 9-15-10
      %warning('Simatra:simex:simCompile', ['Received unexpected code 156'])
  elseif (143 == status)
    simFailure('simCompile', 'simEngine abruptly stopped by kill signal');
  elseif (0 ~= status)
    simFailure('simCompile', ['simEngine internal error (status=' num2str(status) ').']);
  end

  % Collect any outputs from simulation
  if(options.stopTime == options.startTime)
    outputs = [];
    y1 = [];
    t1 = [];
  else 
    [outputs y1 t1] = readSimulationData();
  end

  % Make sure that the interface was produced if not running a simulation
  if(~exist('interface', 'var'))
    if(exist(options.jsonfile, 'file'))
      interface = parseInterface(options);
    else
      simFailure('simCompile', ['No simulation interface available.'])
    end
  end

end
%
function [interface] = parseInterface (options)
  % Read json file
  try
    jsonData = fileread(options.jsonfile);
  catch it
    simFailure('simInterface', 'Could not read interface file.')    
  end
  % Parse json contents
  try
    interface = parse_json(jsonData);
  catch it
    simFailure('simInterface', 'Could not parse interface file.')
  end

  % Convert some fields from strings to numbers
  interface.parallel_models = str2num(interface.parallel_models);
  interface.precision = str2num(interface.precision);
  interface.pointer_size = str2num(interface.pointer_size);
  interface.buffer_length = str2num(interface.buffer_length);

  % Convert default inputs to a structure
  defaultInputs = interface.defaultInputs;
  interface.defaultInputs = {};
  for i = 1:length(defaultInputs)
  % Ensure any "NaN", "-Inf", or "Inf" strings are converted to numbers.
    if isa(defaultInputs{i}, 'char')
      interface.defaultInputs.(interface.inputs{i}) = str2num(defaultInputs{i});
    else
      interface.defaultInputs.(interface.inputs{i}) = defaultInputs{i};  
    end
  end

  % Convert default states to a flat vector
  if(0 == length(interface.defaultStates))
    interface.defaultStates = [];
  else
    defaultStates = interface.defaultStates;
    interface.defaultStates = zeros(1, length(defaultStates));
    for i = 1:length(defaultStates)
      % Ensure any "NaN", "-Inf", or "Inf" strings are converted to numbers.
      if isa(defaultStates{i}, 'char')
        interface.defaultStates(i) = str2num(defaultStates{i});
      else
        interface.defaultStates(i) = defaultStates{i};
      end
    end
  end

  % Convert output sizes to a flat vector
  if(0 == length(interface.outputNumQuantities))
    interface.outputNumQuantities = [];
  else
    outputNumQuantities = interface.outputNumQuantities;
    interface.outputNumQuantities = zeros(1, length(outputNumQuantities));
    for i = 1:length(outputNumQuantities)
      interface.outputNumQuantities(i) = outputNumQuantities{i};
    end
  end
end

%%
function writeUserInputs (options)
%  WRITEUSERINPUTS Creates files for the inputs of each model instance.
  inputs = options.inputs;
  names = fieldnames(inputs);
  fid = 0;
  onCleanup(@()fid > 0 && fclose(fid));

  if ~isstruct(inputs)
    simexError('typeError', 'Expected INPUTS to be a structure array.')
  end

  empty = isempty(names);
  if ~empty
    for inputid = 1:length(names)
      empty = empty || isempty(inputs.(names{inputid}));
    end
  end

  if ~empty
  for inputid = 1:length(names)
    field = names{inputid};
    exist(fullfile(options.outputs, 'inputs'), 'dir') || mkdir(options.outputs, 'inputs');
    filename = fullfile(options.outputs, 'inputs', field);
    fid = fopen(filename, 'w');
    if -1 == fid
      simFailure('simEngine', ['Unable to write inputs file ' filename]);
    end
    
    index = zeros(2, options.instances);

    % Zero over the index region
    fwrite(fid, index, 'integer*4');
    
    if 1 == max(size(inputs))
      value = inputs.(field);
      if iscell(value)
        if 1 == length(value)
          if isnan(value{1})
            simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
          end
          fwrite(fid, value{1}, 'double');
          index(2,:) = length(value{1});
        else
          offset = 0;
          for modelid = 1:options.instances
            if isnan(value{modelid})
              simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
            end
            fwrite(fid, value{modelid}, 'double');
            index(:, modelid) = [offset; length(value{modelid})];
            offset = offset + length(value{modelid});
          end
        end
      elseif isnan(value)
        simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
      else
        fwrite(fid, value, 'double');
        index(2,:) = length(value);
      end
    else
      offset = 0;
      for modelid = 1:options.instances
        value = inputs(modelid).(field);
        fwrite(fid, value, 'double');
        index(:, modelid) = [offset; length(value)]
        offset = offset + length(value);
      end
    end
              
    % Return to the beginning and write the index
    fseek(fid, 0, 'bof');
    fwrite(fid, index, 'integer*4');
    fclose(fid);
  end
  end
end
%%
function writeUserStates (options)
% WRITEUSERSTATES Creates files for the initial states of each model instance.
  states = options.states;
    
  if ~isempty(states)
    filename = fullfile(options.outputs, 'initial-states');
    fid = fopen(filename, 'w');
    onCleanup(@()fid > 0 && fclose(fid));
    if -1 == fid
      simFailure('simEngine', ['Unable to write states file ' filename]);
    end

    if 1 == size(states, 1)
      for modelid = 1:options.instances
        written = fwrite(fid, states, 'double');
      end
    else
      for modelid = 1:options.instances
        written = fwrite(fid, states(modelid,:), 'double');
      end
    end
  end
end
%%

function [running] = processRunning(pid)
    [stat, ignored] = system(['ps -p ' num2str(pid) ' -o pid=']);
    running = not(stat);
end

function cleanupBackgroundProcess(pid)
% kill is called unconditionally, on CTRL+C the simulation is stopped
% For normal execution, the process will have exited and the kill won't do anything
    procs = subprocesses(pid);
    notkilled = 1;
    for p = 1:length(procs)
      command = sprintf('kill -9 %s', num2str(procs(p)));
      [stat, result] = system(command);
      notkilled = notkilled && stat;
    end
    if ~notkilled
        disp('Simulation terminated prematurely.')
	[stat, result] = system('sleep 1');
    end
    readSimulationData(); % Allow mex to clean up internal state
end

function [plist] = subprocesses(pid)
  % Get list of processes from the system by ppid and pid
  [stat, result] = system('ps ax -o ppid=,pid=');
  allprocs = reshape(sscanf(result,'%d'),2,[])';

  plist = pstree(pid, allprocs);
end

function [plist] = pstree(pid, allprocs)
  plist = pid;
  for i = 1:size(allprocs,1)
    if(allprocs(i,1) == pid)
      plist = [plist pstree(allprocs(i,2), allprocs)];
    end
  end
end