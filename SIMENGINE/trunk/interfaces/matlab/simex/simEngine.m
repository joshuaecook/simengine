function [outputs y1 t1 interface] = simEngine (options)
%  SIMENGINE compiles and/or executes a simulation

  writeUserInputs(options);
  writeUserStates(options);

  command = [options.simengine ' --inferior-mode --simex ' options.model ...
             ' --outputdir ' options.outputs ' ' options.args];
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

  system(['touch ' logFile]);
  command = ['(' command ' &>' logFile ' & pid=$! ; echo $pid > ' pidFile ' ; wait $pid; echo $? > ' statusFile ')&'];
  [stat, ignore] = system(command);
  while ~exist(pidFile,'file') || isempty(fileread(pidFile))
    pause(0.1);
  end
  % Ignore the newline
  pid = num2str(str2num(fileread(pidFile)));
  % Remove the file to prevent crosstalk across launchBackground calls
  delete(pidFile);

  c = onCleanup(@()cleanupBackgroundProcess(pid));

  outputlen = 0;
  messagelen = 0;
  while(processRunning(pid))
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
    if(exist('m','var'))
      progress = 100*sum(m.Data)/length(m.Data);
      message = sprintf('Simulating : %0.2f %%', progress);
      messagelen = statusBar(message, messagelen);
    else
      try
        message = fileread(compilationProgressFile);
	messagelen = statusBar(message, messagelen);
      catch it
      end
    end
    try
      log = fileread(logFile);
    catch it
      simFailure('launchBackground', 'Process log file does not exist.')
    end
    if length(log) > outputlen
      fprintf('%s', log(outputlen+1:end));
      outputlen = length(log);
    else
      pause(0.1);
    end
  end
  try
    log = fileread(logFile);
  catch it
    simFailure('launchBackground', 'Process log file does not exist.')
  end
  if length(log) > outputlen
    fprintf('%s', log(outputlen+1:end));
  end
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
    simFailure('launchBackground', 'Process status file does not exist.')      
  end

  if(128 == status)
    simEngineError('simCompile', ['Model ' options.dslfile ' can not be '...
				  'compiled']);
  elseif (129 == status)
    disp(['Please execute ''help simex'' for information on using '...
          'simEngine in MATLAB.'])
      simEngineError('simCompile', ['Can not execute simEngine due to invalid command line options'])
  elseif (0 ~= status)
    simFailure('simCompile', ['SimEngine internal error.']);    
  end

  interface = parseInterface(options);

  if options.stopTime ~= 0  
    [outputs y1 t1] = readSimulationData(interface, options);
  else
    outputs = struct();
    y1 = [];
    t1 = [];
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

  if ~isstruct(inputs)
    simexError('typeError', 'Expected INPUTS to be a structure array.')
  end
    
  for modelid = 1:options.instances
    modelPath = modelidToPath(modelid-1);
    mkdir(fullfile(options.outputs, modelPath), 'inputs');
    for inputid = 1:length(names)
      field = names{inputid};
      if ~isfield(options.inputs, field)
        warning(['INPUTS.' field ' is not a model input.']);
        continue
      end
      filename = fullfile(options.outputs, modelPath, 'inputs', field);
      fid = fopen(filename, 'w');
      onCleanup(@()fid>0 && fclose(fid));
            
      if -1 == fid
        simFailure('simEngine', ['Unable to write inputs file ' filename]);
      end
            
      if 1 == max(size(inputs))
        % INPUTS given as a structure of scalars or cell arrays
        value = inputs.(field);
        if iscell(value)
          if 1 == length(value)
            if isnan(value{1})
              simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
            end
            fwrite(fid, value{1}, 'double');
          else
            if isnan(value{modelid})
              simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
            end
            fwrite(fid, value{modelid}, 'double');
          end
        elseif isnan(value)
          simexError('valueError', ['INPUTS.' field ' may not contain NaN.']);
        else
          fwrite(fid, value, 'double');
        end
      else
        value = inputs(modelid).(field);
        fwrite(fid, value, 'double');
      end
    end
  end
end
%%
function writeUserStates (options)
% WRITEUSERSTATES Creates files for the initial states of each model instance.
  states = options.states;
    
  if ~isempty(states)
    for modelid = 1:options.instances
      modelPath = modelidToPath(modelid-1);
      filename = fullfile(options.outputs, modelPath, 'initial-states');
      fid = fopen(filename, 'w');
      onCleanup(@()fid>0 && fclose(fid));
      if -1 == fid
        simFailure('simEngine', ['Unable to write states file ' filename]);
      end
            
      if 1 == size(states, 1)
        fwrite(fid, states, 'double');
      else
        fwrite(fid, states(modelid,:), 'double');
      end
    end
  end
end
%%
function [val] = byte(number, b)
    val = bitand(bitshift(number, -(b*8)),255);
end

function [path] = modelidToPath(modelid)
    path = sprintf('%02x%s%02x%s%02x', byte(modelid,2), filesep, byte(modelid,1), filesep, byte(modelid,0));
end

function [running] = processRunning(pid)
    [stat, ignored] = system(['ps -p ' pid ' -o pid=']);
    running = not(stat);
end

function cleanupBackgroundProcess(pid)
% kill is called unconditionally, on CTRL+C the simulation is stopped
% For normal execution, the process will have exited and the kill won't do anything
    command = sprintf('kill -9 %s', pid);
    [stat, result] = system(command);
    if ~stat
        disp('User terminated simulation.')
    end
end