function [outputs y1 t1] = simEngine (interface, options)
%  SIMENGINE Executes a compiled simulation
    writeUserInputs(options);
    writeUserStates(options);
        
    [outputs y1 t1] = simulateModel(options);
end

%%
function writeUserInputs (options)
%  WRITEUSERINPUTS Creates files for the inputs of each model instance.
    inputs = options.inputs;
    
    if ~isstruct(inputs)
        simexError('typeError', 'Expected INPUTS to be a structure array.')
    end

    multiply = @(value)(value * ones(options.instances, 1));
    
    names = fieldnames(inputs);
    
    if 1 == max(size(inputs))
        %% INPUTS given as a struct of scalars or cell arrays
        for modelid = 1:options.instances
            modelPath = modelidToPath(modelid-1);
            mkdir(fullfile(options.outputs, modelPath), 'inputs');
            for inputid = 1:length(names)
                filename = fullfile(options.outputs, modelPath, 'inputs', names{inputid});
                fid = fopen(filename, 'w');
                if -1 == fid
                    simFailure('simEngine', ['Unable to write inputs file ' filename]);
                end
                
                value = inputs.(names{inputid});
                if iscell(value)
                    if 1 == length(value)
                        fwrite(fid, value{1}, 'double');
                    else
                        fwrite(fid, value{modelid}, 'double');
                    end
                else
                    fwrite(fid, value, 'double');
                end
                fclose(fid);
            end
        end
    else
        simexError('argumentError', 'Unexpected dimensions of INPUTS.');
    end
end
%%
function writeUserStates (options)
% WRITEUSERSTATES Creates files for the initial states of each model instance.
    states = options.states;
    
    if ~isempty(states)
        filename = fullfile(options.outputs, 'states');
        fid = fopen(filename, 'w');
        if -1 == fid
            simFailure('simEngine', ['Unable to write states file ' filename]);
        end

        for modelid = 1:options.instances
            % Uses the 'skip' parameter of fwrite() to transpose while writing.
            if 1 == size(states, 1)
                fwrite(fid, states, 'double', 8 * options.instances);
            else
                fwrite(fid, states(modelid,:), 'double', 8 * options.instances);
            end
        end
    end
end
function [outputs y1 t1] = readSimulationData (options)
% READSIMULATIONDATA Reads the outputs, final states, and final
% times files by memory mapping.

    outputs = struct();
    % Empty data structures to be filled in before returning
    y1 = zeros(options.instances, length(interface.states));
    t1 = zeros(1, options.instances);
    
    for modelid = 1:options.instances
        for outputid = 1:length(interface.outputs)
            modelDir = fullfile(options.outputs, modelidToPath(modelid-1));
            outputFile = fullfile(modelDir, interface.outputs{outputid});
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
function [outputs y1 t1] = simulateModel(opts)
  opts.args = [opts.args ' --start ' num2str(opts.startTime)];
  opts.args = [opts.args ' --stop ' num2str(opts.stopTime)];
  opts.args = [opts.args ' --instances ' num2str(opts.instances)];
  opts.args = [opts.args ' --startupmessage=false'];
  
  simCompile(opts, 'Simulating');
  
  [outputs y1 t1] = readSimulationData(opts);
end
%%
function [val] = stringByte(number, b)
  val = num2str(bitand(bitshift(number, -(b*8)),255), '%02x');
end

function [path] = modelidToPath(modelid)
  path = fullfile(stringByte(modelid,2),stringByte(modelid,1),stringByte(modelid,0));
end
