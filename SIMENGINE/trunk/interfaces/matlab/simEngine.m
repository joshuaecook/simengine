function [outputs y1 t1] = simEngine (interface, options)
%  SIMENGINE Executes a compiled simulation
    userInputs = vet_user_inputs(interface, options.inputs);
    userStates = vet_user_states(interface, options.states);
    
    inputsM = size(userInputs,1);
    statesM = size(userStates,1);

    if 1 < inputsM && 1 < statesM && inputsM ~= statesM
        simexError('argumentError', ...
                   'When INPUTS and Y0 both contain more than 1 row, they must have the same number of rows.');
    end

    % Write inputs to file
    if 0 < inputsM
        inputsFile = fullfile(options.outputs, 'inputs');
        options.args = [options.args ' --inputs ' inputsFile];
        inputFileID = fopen(inputsFile, 'w');
        if -1 == inputFileID
            simFailure('writeInputs', ['Could not open inputs file: ' inputsFile]);
        end
        for i = size(userInputs, 1):options.instances
            fwrite(inputFileID, userInputs', 'double');
        end
        fclose(inputFileID);
    end

    % Write states to file
    if 0 < statesM
        statesFile = fullfile(options.outputs, 'states');
        options.args = [options.args ' --states ' statesFile];
        stateFileID = fopen(statesFile, 'w');
        if -1 == stateFileID
            simFailure('writeStates', ['Could not open inputs file: ' statesFile]);
        end
        for i = size(userStates,1):options.instances
            fwrite(stateFileID, userStates', 'double');
        end
        fclose(stateFileID);
    end

    simulate_model(options);
    
    % Empty data structures to be filled in before returning
    outputs = struct();
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
function [userInputs] = vet_user_inputs(interface, inputs)
%  VET_USER_INPUTS Verifies that the user-supplied inputs are valid.
%     Returns a MxN matrix where N is the number of model inputs.
%     M is the number of parallel models.
    if ~isstruct(inputs)
        simexError('typeError', ...
                   'Expected INPUTS to be a structure.')
    end
    
    [rows cols] = size(inputs);
    if 2 < ndims(inputs)
        simexError('argumentError', ['INPUTS may not have more than 2 dimensions.']);
    elseif 1 ~= min([rows cols])
        simexError('argumentError', ['INPUTS must have one scalar dimension.']);
    end
    
    models = max([rows cols]);
    fields = fieldnames(inputs);

    if 1 == models
        %% INPUTS given as a struct of cell arrays
        
        for fieldid=1:length(fields)
            field = fields{fieldid};
            if ~isfield(interface.defaultInputs, field)
                warning([field ' is not defined as a model input.']);
            end
            
            value = inputs.(field);
            
            if iscell(value)
                [rows cols] = size(value);
                if 2 < ndims(field)
                    simexError('valueError', ['INPUTS.' fieldname ' may not have more than 2 dimensions.']);
                elseif 1 ~= min([rows cols])
                    simexError('valueError', ['INPUTS.' field ' must have one scalar dimension.']);
                end

                if 1 == models
                    models = max([rows cols]);
                elseif models ~= max([rows cols])
                    simexError('valueError', 'All cell array INPUT fields must have the same length.');
                end
            elseif ~isscalar(value)
                simexError('valueError', ['INPUTS.' field ' must be a scalar or cell array.']);
            end
        end
        
                        
        userInputs = zeros(models, length(interface.inputs));
        multiply = @(value)(value * ones(models, 1));

        fields = interface.inputs;
        for fieldid=1:length(fields)
            field = fields{fieldid};

            if ~isfield(inputs, field)
                value = interface.defaultInputs.(field);
                if isnan(value)
                    simexError('valueError', ['Model input ' field ' has no default value and must be specified.']);
                end
                userInputs(1:models, fieldid) = multiply(value);
                continue
            end

            value = inputs.(field);
            if isscalar(value)
                userInputs(1:models, fieldid) = multiply(double(value));
                if isnan(value)
                    simexError('valueError', ['INPUTS.' field ' may not be NaN.']);
                end
            elseif iscell(value) && length(value) == models
                % This assumes all elements of the cell are scalar.
                % This will have to change when inputs are vectorized.
                userInputs(1:models, fieldid) = double(cell2mat(value));
                if any(isnan(userInputs(1:models, fieldid)))
                    simexError('valueError', ['INPUTS.' field ' may not be NaN.']);
                end
            else
                simexError('valueError', ['Expected INPUTS.' field ' to be scalar or a cell array of length ' num2str(models) '.']);
            end
        end
    else
        %% INPUTS given as a struct array of scalars
        % TODO
    end
end

%%
function [userStates] = vet_user_states(interface, states)
%  VET_USER_STATES Verifies that the user-supplied initial states
%  contain valid data.
    if ~isnumeric(states)
        simexError('typeError', 'Expected Y0 to be numeric.');
    elseif issparse(states)
        simexError('typeError', 'Did not expect Y0 to be sparse.');
        %elseif iscomplex(states)
        %  warning('Simatra:warning', 'Ignoring imaginary components of Y0.');
    elseif any(isnan(states))
        simexError('valueError', 'Y0 may not contain NaN values.');
    end

    [statesRows statesCols] = size(states);
    userStates = [];

    if 0 < statesRows && 0 < statesCols
        if statesCols ~= length(interface.states)
            simexError('argumentError', ...
                       ['Y0 must contain ' length(interface.states) ' columns.']);
        end
        userStates = double(states);
    end
end

%%
function [] = simulate_model(opts)
  opts.args = [opts.args ' --start ' num2str(opts.startTime)];
  opts.args = [opts.args ' --stop ' num2str(opts.stopTime)];
  opts.args = [opts.args ' --instances ' num2str(opts.instances)];
  opts.args = [opts.args ' --startupmessage=false'];
  
  simCompile(opts, 'Simulating');
end
%%
function [val] = stringByte(number, b)
  val = num2str(bitand(bitshift(number, -(b*8)),255), '%02x');
end

function [path] = modelidToPath(modelid)
  path = fullfile(stringByte(modelid,2),stringByte(modelid,1),stringByte(modelid,0));
end
