function [outputs y1 t1] = simEngine (interface, options)
%  SIMENGINE Executes a compiled simulation
    writeUserInputs(interface, options);
    writeUserStates(interface, options);
    
    [outputs y1 t1] = simulateModel(interface, options);
end

%%
function writeUserInputs (interface, options)
%  WRITEUSERINPUTS Creates files for the inputs of each model instance.
    inputs = options.inputs;
    
    if ~isstruct(inputs)
        simexError('typeError', 'Expected INPUTS to be a structure array.')
    end
    
    names = interface.inputs;
    
    for inputid = 1:length(names)
        field = names{inputid};
        if isnan(interface.defaultInputs.(field)) && ~isfield(inputs,field)
            simexError('valueError', ['Model input ' field ' has no default value and must be specified.']);
        end
    end

    names = fieldnames(inputs);
    
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
            fclose(fid);
        end
    end
end
%%
function writeUserStates (interface, options)
% WRITEUSERSTATES Creates files for the initial states of each model instance.
    states = options.states;
    
    if ~isempty(states)
        for modelid = 1:options.instances
            modelPath = modelidToPath(modelid-1);
            filename = fullfile(options.outputs, modelPath, 'states');
            fid = fopen(filename, 'w');
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
function [outputs y1 t1] = readSimulationData (interface, options)
% READSIMULATIONDATA Reads the outputs, final states, and final
% times files by memory mapping.

    outputs = struct();
    % Empty data structures to be filled in before returning
    y1 = zeros(options.instances, length(interface.states));
    t1 = zeros(1, options.instances);
    
    for modelid = 1:options.instances
        for outputid = 1:length(interface.outputs)
            modelDir = fullfile(options.outputs, modelidToPath(modelid-1));
            outputFile = fullfile(modelDir, 'outputs', interface.outputs{outputid});
            try
                m = memmapfile(outputFile, 'format', 'double');
                outputs(modelid).(interface.outputs{outputid}) = reshape(m.Data, interface.outputNumQuantities(outputid), [])';
            catch it
                % this means there is no data in the output file which can happen for conditional outputs
                outputs(modelid).(interface.outputs{outputid}) = [];
            end
            try
                if(~isempty(interface.states))
                    finalStatesFile = fullfile(modelDir, 'final-states');
                    m = memmapfile(finalStatesFile, 'format', 'double');
                    y1(modelid,:) = m.Data;
                end
                finalTimeFile = fullfile(modelDir, 'final-time');
                m = memmapfile(finalTimeFile, 'format', 'double');
                t1(modelid) = m.Data;
            catch it
                simFailure('finishSim', ['Simulation did not finish, final time was not reached for model instance ' num2str(modelid) '.'])
            end
        end
    end
end
%%
function [outputs y1 t1] = simulateModel(interface, opts)
  opts.args = [opts.args ' --start ' num2str(opts.startTime)];
  opts.args = [opts.args ' --stop ' num2str(opts.stopTime)];
  opts.args = [opts.args ' --instances ' num2str(opts.instances)];
  opts.args = [opts.args ' --startupmessage=false'];
  
  simCompile(opts, 'Simulating');
  
  [outputs y1 t1] = readSimulationData(interface, opts);
end
%%
function [val] = stringByte(number, b)
  val = num2str(bitand(bitshift(number, -(b*8)),255), '%02x');
end

function [path] = modelidToPath(modelid)
  path = fullfile(stringByte(modelid,2),stringByte(modelid,1),stringByte(modelid,0));
end
