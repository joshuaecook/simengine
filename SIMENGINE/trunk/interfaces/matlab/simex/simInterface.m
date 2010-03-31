function [interface] = simInterface (options)
%  SIMINTERFACE Returns a struct representing the interface of a
%  simulation.
    jsonPath = fullfile(options.outputs, 'simex_interface.json');
    options.args = [options.args ' --json_interface ' jsonPath];
    simCompile(options, 'Compiling');
    try
        jsonData = fileread(jsonPath);
    catch it
        simFailure('simInterface', 'Could not read interface file.')    
    end
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

    % Convert output sizes to a flat vector
    outputNumQuantities = interface.outputNumQuantities;
    interface.outputNumQuantities = zeros(1, length(outputNumQuantities));
    for i = 1:length(outputNumQuantities)
        interface.outputNumQuantities(i) = outputNumQuantities{i};
    end

    % Remove fields that have no meaning to user
    interface = rmfield(interface, {'hashcode', 'version'});
end
