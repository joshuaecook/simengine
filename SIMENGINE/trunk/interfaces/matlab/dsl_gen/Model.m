classdef Model < handle
% Model - Programmatically build DSL models for simEngine
%
% Model Methods:
%   Constructor:
%   Model - create a new model object
%
%   Model Elements: 
%   input - define a model input
%   state - define a state of the system
%   output - define a model output
%   equ - define an intermediate equation
%   diffequ - define a differential equation
%   submodel - instantiate a sub model
%
%   Model Processing:
%   type - display the generated model
%   toDSL - write the model to a file
%
% Copyright 2010 Simatra Modeling Technologies
% Website: www.simatratechnologies.com
% Support: support@simatratechnologies.com
%
    properties (Access = protected)
        Name
        Inputs
        Outputs
        States
        Instances
        IntermediateEqs
        IntermediateEqsNames
        DiffEqs
    end
    
    properties (Access = private)
        instance_number = 1
        state_number = 1
        intermediate_number = uint32(1)
        cachedModels
    end
    
    methods
        function m = Model(Name)
            % MODEL - create a new Model object
            % 
            % Usage:
            %   m = MODEL(modelname) - generate a new model with name
            %   modelname
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %

            m.Name = Name;
            m.Inputs = containers.Map;
            m.Outputs = containers.Map;
            m.States = containers.Map;
            m.Instances = containers.Map;
            m.IntermediateEqs = containers.Map('KeyType','uint32','ValueType','Any');
            m.IntermediateEqsNames = containers.Map;
            m.DiffEqs = struct();
            m.cachedModels = containers.Map;
        end
        
        function e = state(m, varargin)
            
            % default arguments
            init = false;
            id = ['InternalState__' num2str(m.state_number)];
            m.state_number = m.state_number + 1;
            iterator = false;
            
            args = varargin;
            i = 1; % argument counter
            while ~isempty(args)
                arg = args{1};
                if 1 == i && ischar(arg)
                    id = arg;
                    i = i + 1;
                elseif isnumeric(arg) || isa(arg, 'Exp')
                    init = arg;
                    i = i + 1;
                elseif ischar(arg) && strcmpi(arg, 'iter') && length(args)>1
                    iterator = args{2};
                    if ~isa(iterator, 'Iterator')
                        error('Simatra:Model:state', '''iter'' argument expects an Iterator type')
                    end
                    i = i + 2;
                else
                    error('Simatra:Model', 'Unexpected argument to state')
                end
                args = varargin(i:end);
            end
            
            if isfield(m.States, id)
                error('Simatra:Model', ['State ' id ' already exists']);
            else
                e = Exp(id);
                m.States(id) = struct('init', init, 'iterator', iterator);
            end
    
        end

        
        function e = input(m, id, default)
            if isfield(m.Inputs, id)
                error('Simatra:Model', ['Input ' id ' already exists']);
            else
                e = Exp(id);
                if nargin == 3
                    m.Inputs(id) = struct('default', default);
                else
                    m.Inputs(id) = struct();
                end
            end
        end
        
        function output(m, id, varargin)
            condition = false;
            iterator = false;
            if 2 == nargin
                if ischar(id)
                    if (m.Inputs.isKey(id) || ...
                        m.States.isKey(id) || ...
                        m.IntermediateEqsNames.isKey(id))
                        e = {Exp(id)};
                    else
                        msg = ['Variable ' id ' has not been defined in the system'];
                        error('Simatra:Model', msg)
                    end
                else
                    error('Simatra:Model', 'The first argument to the output method but be a string variable name.')
                end
            elseif 3 == nargin
                if iscell(varargin{1})
                    e = varargin{1};
                else
                    e = varargin;
                end
            else
                i = 1;
                j = 1;
                e = {};
                args = varargin;
                while ~isempty(args);
                    arg = args{1};
                    if ischar(arg) && strcmpi(arg,'when') && length(args) > 1
                        condition = args{2};
                        if ~isa(condition, 'Exp')
                            error('Simatra:Model:output', 'Expected an expression argument for the ''when'' condition')
                        end
                        i = i + 2;
                    elseif ischar(arg) && strcmpi(arg,'iter') && length(args) > 1
                        iterator = args{2};
                        if ~isa(iterator, 'Iterator')
                            error('Simatra:Model:output', 'Expected an iterator argument')
                        end
                        i = i + 2;
                    else
                        if iscell(varargin{i})
                            for k=1:length(varargin{i})
                                e{j} = varargin{i}{k};
                                j = j + 1;
                            end
                        else
                            e{j} = varargin{i};
                            j = j + 1;
                        end
                        i = i + 1;
                    end
                    args = varargin(i:end);
                end
            end
            
            if m.Outputs.isKey(id)
                error('Simatra:Model', ['Output ' id ' already exists']);
            else
                m.Outputs(id) = struct('contents', {e}, 'condition', condition, 'iterator', iterator);    
            end
        end
        
        function instance = submodel(m, modelarg)
            if ischar(modelarg) && exist(modelarg, 'file')
                [filepath, modelname, fileext] = fileparts(modelarg);
                if m.cachedModels.isKey(modelname)
                    name = modelname;
                    name = m.cachedModels(modelname).name;
                    inputs = m.cachedModels(modelname).inputs;
                    outputs = m.cachedModels(modelname).outputs;
                else
                    disp(['Evaluating ' modelname ' ...'])
                    minfo = simex(modelarg);
                    name = minfo.name;
                    inputs = minfo.inputs;
                    outputs = minfo.outputs;
                    modelStruct = struct('name', name, 'filename', modelarg, ...
                        'inputs', {inputs}, 'outputs', {outputs});
                    m.cachedModels(modelname) = modelStruct;
                end
            elseif isa(modelarg,'Model')
                name = modelarg.Name;
                if m.cachedModels.isKey(name)
                    model = m.cachedModels(name);
                    inputs = model.inputs;
                    outputs = model.outputs;
                else
                    inputs = keys(modelarg.Inputs);
                    outputs = keys(modelarg.Outputs);
                    modelStruct = struct('name', name, 'model', modelarg, ...
                        'inputs', {inputs}, 'outputs', {outputs});
                    m.cachedModels(name) = modelStruct;
                end
            else
                error('Simatra:Model', 'Unexpected instance')
            end
            inst = ['Instance_' num2str(m.instance_number)];
            m.instance_number = m.instance_number + 1;
            m.Instances(inst) = struct('name', name, 'inputs', {inputs}, 'outputs', {outputs}, 'obj', modelarg);
            instance = Instance(inst, m, inputs, outputs);
        end

        function e = equ(m, lhs, rhs)
            if 2 == nargin
                rhs = lhs;
                lhsstr = ['InternalIntemediate__' num2str(m.intermediate_number)];
                lhs = Exp(lhsstr);
            else
                if isa(lhs, 'Exp')
                    lhsstr = toId(lhs);
                else
                    lhsstr = lhs;
                    lhs = Exp(lhs);
                end
            end
            if m.IntermediateEqsNames.isKey(lhsstr)
                error('Simatra:Model', ['Equation assigning ' lhs ' already exists']);
            else
                e = Exp(lhsstr);
                m.IntermediateEqs(m.intermediate_number) = struct('lhs', lhs, 'rhs', rhs);
                m.IntermediateEqsNames(lhsstr) = m.intermediate_number;
            end
            m.intermediate_number = m.intermediate_number + 1;
        end
        
        
        function diffequ(m, lhs, rhs)
            id = toStr(lhs);
            if isfield(m.DiffEqs, id)
                error('Simatra:Model', ['Differential Equation assigning ' lhs ' already exists']);
            else
                m.DiffEqs.(id) = struct('lhs', lhs, 'rhs', rhs);    
            end
        end        

        function map = findIterators(m)
            map = containers.Map;
            % Search for iterators everywhere
            %  - first in intermediate equations
            structs = values(m.IntermediateEqs);
            for i=1:length(structs)
                map = findIterators(structs{i}.rhs, map);
            end
            %  - Next in differential equations
            ids = fieldnames(m.DiffEqs);
            for i=1:length(ids)
                map = findIterators(m.DiffEqs.(ids{i}).rhs, map);
            end
            %  - Next in outputs
            structs = values(m.Outputs);
            for i=1:length(structs)
                contents = structs{i}.contents;
                condition = structs{i}.condition;
                for j=1:length(contents)
                    map = findIterators(contents{j}, map);
                end
                if isa(condition, 'Exp')
                    map = findIterators(structs{i}.condition, map);
                end
                iter = structs{i}.iterator;
                if isa(iter, 'Iterator')
                    map(iter.id) = iter;
                end
            end
            % - Search through states
            structs = values(m.States);
            for i=1:length(structs)
                iter = structs{i}.iterator;
                if isa(iter, 'Iterator')
                    map(iter.id) = iter;
                end
            end
            % ADD INPUTS WHEN READY
        end

        function str = toStr(m)
            inputs = keys(m.Inputs);
            outputs = keys(m.Outputs);
            states = keys(m.States);
            eqs = keys(m.IntermediateEqs);
            eqsNames = keys(m.IntermediateEqsNames);
            diffeqs = fieldnames(m.DiffEqs);
            instances = keys(m.Instances);
            cachedmodels = keys(m.cachedModels);
            iterators = values(findIterators(m));
            
            str = '';
            str = [str '// Generated DSL model: ' m.Name '\n'];
            %str = [str '// Created: ' datestr(now) '\n'];
            str = [str '// Copyright 2010 Simatra Modeling Technologies\n'];
            str = [str '\n'];
            str = [str '// Import List\n'];
            for i=1:length(cachedmodels)
                if isfield(m.cachedModels(cachedmodels{i}), 'filename')
                    str = [str 'import "' m.cachedModels(cachedmodels{i}).filename '"' '\n'];
                elseif isfield(m.cachedModels(cachedmodels{i}), 'model')
                    str = [str toStr(m.cachedModels(cachedmodels{i}).model) '\n'];
                else
                    error('Simatra:Model', ['Unexpected model type on model ' cachedmodels{i}]);
                end
            end
            str = [str '\n'];                
            outputList = ['(' concatWith(', ', outputs) ')'];
            inputList = ['(' concatWith(', ', inputs), ')'];
            str = [str 'model ' outputList ' = ' m.Name inputList '\n'];
            str = [str '\n'];
            str = [str '   // Iterator definitions\n'];
            for i=1:length(iterators)
                str = [str '   ' iterators{i}.toStr '\n'];
            end
            str = [str '\n'];
            str = [str '   // Input definitions\n'];
            for i=1:length(inputs)
                input = m.Inputs(inputs{i});
                if isfield(input, 'default')
                    attributes = [' with {default=' num2str(input.default) '}'];
                else
                    attributes = '';
                end
                str = [str '   input ' inputs{i} attributes '\n'];
            end
            str = [str '\n'];
            str = [str '   // State definitions\n'];
            for i=1:length(states)
                state = m.States(states{i});
                iter_str = '';
                if isa(state.iterator, 'Iterator')
                    iter_str = [' with {iter=' state.iterator.id '}'];
                end
                str = [str '   state ' states{i} ' = ' num2str(state.init) iter_str '\n'];
            end
            str = [str '\n'];
            str = [str '   // Instance definitions\n'];
            for i=1:length(instances)
                inst = m.Instances(instances{i});
                name = inst.name;
                str = [str '   submodel ' name ' ' instances{i} '\n'];
            end
            str = [str '\n'];
            str = [str '   // Equation definitions\n'];
            for i=1:length(eqs)
                index = eqs{i};
                equ = m.IntermediateEqs(index);
                lhs = equ.lhs;
                rhs = equ.rhs;
                if isRef(lhs)
                    str = [str '   ' toStr(lhs) ' = ' toStr(rhs) '\n'];
                else
                    str = [str '   equation ' toStr(lhs) ' = ' toStr(rhs) '\n'];
                end
            end
            str = [str '\n'];
            str = [str '   // Differential equation definitions\n'];
            str = [str '   equations\n'];
            for i=1:length(diffeqs)
                lhs = diffeqs{i};
                rhs = m.DiffEqs.(lhs).rhs;
                str = [str '      ' lhs ''' = ' toStr(rhs) '\n'];
            end
            str = [str '   end\n'];
            str = [str '\n'];
            str = [str '   // Output definitions\n'];
            for i=1:length(outputs)
                name = outputs{i};
                output = m.Outputs(name);
                contents = output.contents;                
                contentsStr = cell(1,length(contents));
                for j=1:length(contents);
                    contentsStr{j} = contents{j}.toStr;
                end
                contentsList = ['(' concatWith(', ', contentsStr), ')'];
                optcondition = '';
                if isa(output.condition, 'Exp')
                    optcondition = [' when ' output.condition.toStr];
                end
                iter = output.iterator;
                iter_str = '';
                if isa(iter, 'Iterator')
                    iter_str = ['[' iter.id ']'];
                end
                str = [str '   output ' name iter_str ' = ' contentsList optcondition '\n'];
            end
            str = [str 'end\n'];
        end
        
        
        function filename = toDSL(m)
            disp(['Creating ' m.Name ' ...']);
            str = toStr(m);
            filename = fullfile(tempdir, [m.Name '.dsl']);
            
            writeFile = true;
            if exist(filename, 'file')
                str_from_file = fileread(filename);
                str_from_file = regexprep(str_from_file, char(10), '\\n');
                if strcmp(str, str_from_file)
                    disp(['Reusing file ' filename]);
                    writeFile = false;
                else
                    %disp(sprintf('files do not match (length: %d != %d)', length(str), length(str_from_file)));
                end
            else
                %disp(sprintf('file %s does not exist', filename));
            end
            if writeFile
                fid = fopen(filename, 'w');
                fprintf(fid, str);
                fclose(fid);
            end
        end
        
        function type(m)
            str = toStr(m);
            fprintf(1, str);
        end
        
    end
    
end

function s = concatWith(tok, cellarray)
s = '';
if ~isempty(cellarray)
    for i=1:(length(cellarray)-1)
        s = [s cellarray{i} tok];
    end
    s = [s cellarray{end}];
end
end


