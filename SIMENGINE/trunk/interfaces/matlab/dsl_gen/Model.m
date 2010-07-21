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
%   random - define a random number
%   equ - define an intermediate equation
%   diffequ - define a differential equation
%   recurrenceequ - define a recurrence (difference) equation
%   update - define when a state variable updates
%   submodel - instantiate a sub model
%
%   Model Simulation:
%   simex - execute the simulation of the model
%
%   Model Processing:
%   type - display the generated model
%   toDSL - write the model to a file
%
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
        RecurrenceEqs
        Randoms
        DefaultIterator
    end
    
    properties (Access = public)
        solver = {'ode45', 'dt', 0.1};
    end
    
    properties (Access = private)
        instance_number = 1
        state_number = 1
        intermediate_number = uint32(1)
        cachedModels
    end
    
    methods
        function m = Model(Name, iterator)
            % MODEL - create a new Model object
            % 
            % Usage:
            %   m = MODEL(modelname) - generate a new model with name
            %   modelname
            %
            %   m = MODEL(modelname, iterator) - generate a new model with
            %   a specified default iterator
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %

            switch nargin
                case 0
                    error('Simatra:Model', 'Must supply at least one argument');
                case 1
                    if ~ischar(Name)
                        error('Simatra:Model', 'First argument to Model constructor must be a string');
                    end
                    m.DefaultIterator = Iterator('ModelTime', 'continuous', 'solver', m.solver{:});
                case 2
                    if isa(iterator, 'Iterator')
                        m.DefaultIterator = iterator;
                    else
                        error('Simatra:Model', 'Second argument to Model constructor must be an iterator');                        
                    end
                otherwise
                    error('Simatra:Model', 'Must supply no more than two arguments to Model');
            end
                    
            m.Name = Name;
            m.initializeModel;
        end
        
        function e = state(m, varargin)
            % MODEL/STATE - create a new state variable in a Model object
            % 
            % Usage:
            %   s = mdl.STATE(INIT) - generate a state variable with initial
            %   value INIT, return the quantity s
            %
            %   s = mdl.STATE(ID, INIT) - generate the state variable but give
            %   it the explicit name ID
            %
            %   s = mdl.STATE([ID ,], INIT, 'iter', ITERATOR) - explicitly set
            %   an iterator to the state.  Otherwise, it will use the
            %   iterator assigned to MDL.DefaultIterator.
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            % default arguments
            init = false;
            id = ['InternalState__' num2str(m.state_number)];
            m.state_number = m.state_number + 1;
            iterator = m.DefaultIterator;
            
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
            
            if identifier_exists(m, id)
                error('Simatra:Model', ['Identifier ' id ' already defined']);
            else
                e = Exp(id);
                m.States(id) = struct('init', init, 'iterator', iterator);
            end
    
        end

        
       function e = random(m, varargin)
            % MODEL/RANDOM - create a new random number generator in a
            % model object
            % 
            % Usage:
            %   r = mdl.RANDOM('uniform', ['low', LOW, ] ['high', HIGH]) -
            %   return a uniform random number generator that on each
            %   iteration will produce values between LOW and HIGH.  The
            %   default LOW value is 0, the default HIGH value is 1.
            %
            %   r = mdl.RANDOM('normal', ['mean', MEAN, ] ['stddev',
            %   STDDEV]) - return a normal (Guassian) distributed random
            %   number each iteration.  MEAN is set to 0 and STDDEV is set
            %   to 1 by default.
            %
            %   r = mdl.RANDOM(..., 'iter', ITERATOR) - explicitly set
            %   an iterator to the random number. A new random number will
            %   be generated according to the specified iterator
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            % default arguments
            id = ['InternalRandomState__' num2str(m.state_number)];
            m.state_number = m.state_number + 1;
            iterator = m.DefaultIterator;
            uniform = true;
            mean = 0;
            stddev = 1;
            high = 1;
            low = 0;
            
            args = varargin;
            i = 1; % argument counter
            while ~isempty(args)
                arg = args{1};
                if ischar(arg) && strcmpi(arg, 'uniform')
                  uniform = true; 
                  i = i + 1;
                elseif ischar(arg) && strcmpi(arg, 'normal')
                  uniform = false;
                  i = i + 1;
                elseif ischar(arg) && strcmpi(arg, 'iter') && length(args)>1
                    iterator = args{2};
                    if ~isa(iterator, 'Iterator')
                        error('Simatra:Model:state', '''iter'' argument expects an Iterator type')
                    end
                    i = i + 2;
                elseif ischar(arg) && length(args)>1 && ...
                      isnumeric(args{2})
                  val = args{2};
                  if strcmpi(arg, 'mean')
                    mean = val;
                  elseif strcmpi(arg, 'stddev')
                    stddev = val;
                  elseif strcmpi(arg, 'high')
                    high = val;
                  elseif strcmpi(arg, 'low')
                    low = val;
                  else
                    error('Simatra:Model:random', ['Unexpected ' ...
                                        'argument %s'], arg);
                  end
                  i = i + 2;
                else
                    error('Simatra:Model', 'Unexpected argument to state')
                end
                args = varargin(i:end);
            end
            
            if m.Randoms.isKey(id)
                error('Simatra:Model', ['Random ' id ' already exists']);
            else
                e = Exp(id);
                s = struct('iterator', iterator);
                if uniform
                  s.uniform = true;
                  s.high = high;
                  s.low = low;
                else
                  s.uniform = false;
                  s.mean = mean;
                  s.stddev = stddev;
                end
                m.Randoms(id) = s;
            end
    
        end

        
        function e = input(m, id, default)
            if identifier_exists(m, id)
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
                    if (identifier_exists(m, id))
                        e = {Exp(id)};
                        % This is the case if you specify a string output name
                        % and that variable exists inside the model with that
                        % name
                    elseif evalin('caller',['exist(''' id ''',''var'')'])
                        % This is the case if you specify a string output
                        % name and that variable does not exist by name in
                        % the model, but it does exist in the workspace as
                        % a variable.
                        e = {Exp(evalin('caller',id))};
                    else
                        msg = ['Variable ' id ' has not been defined in the system'];
                        error('Simatra:Model', msg)
                    end
                elseif isa(id, 'Exp') || isa(id, 'Iterator')
                    % Finally, this is the case where you specify an
                    % expression to be output and it has to determine the
                    % name from the inputname command
                    e = {Exp(id)};
                    id = inputname(2);
                elseif isnumeric(id)
                    % This is a degenerate case where we would like to
                    % output a constant
                    e = {Exp(id)};
                    id = inputname(2);
                else
                    error('Simatra:Model', 'The first argument to the output method must be a string variable name.')
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
            e = List.map(@(elem)(Exp(elem)), e);
            if m.Outputs.isKey(id)
                error('Simatra:Model', ['Output ' id ' already exists']);
            else
                m.Outputs(id) = struct('contents', {e}, 'condition', condition, 'iterator', iterator);    
            end
        end
        
        function instance = submodel(m, arg1, arg2)
            if nargin == 2
                inst = ['Instance_' num2str(m.instance_number)];
                m.instance_number = m.instance_number + 1;
                modelarg = arg1;
            elseif nargin == 3
                inst = arg1;
                if identifier_exists(m, inst)
                    error('Simatra:Model:submodel', 'Identifier %s already defined', inst);
                end
                modelarg = arg2;
            else
                error('Simatra:Model:submodel:ArgumentError', 'Wrong number of arguments');
            end
            
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
            m.Instances(inst) = struct('name', name, 'inputs', {inputs}, 'outputs', {outputs}, 'obj', modelarg);
            instance = Instance(inst, m, inputs, outputs);
        end

        function e = equ(m, lhs, rhs)
            if 2 == nargin
                rhs = Exp(lhs);
                lhsstr = ['InternalIntermediate__' num2str(m.intermediate_number)];
                lhs = Exp(lhsstr);
            else
                if isa(lhs, 'Exp')
                    lhsstr = toId(lhs);
                elseif ischar(lhs)
                    lhsstr = lhs;
                    lhs = Exp(lhs);
                else
                    error('Simatra:Model:equ', 'First argument to EQU must be a string or an Exp type');
                end
            end
            if identifier_exists(m, lhsstr)
                error('Simatra:Model:equ', ['Variable ' lhsstr ' has already been defined']);
            else
                e = Exp(lhsstr);
                m.IntermediateEqs(m.intermediate_number) = struct('lhs', lhs, 'rhs', rhs);
                m.IntermediateEqsNames(lhsstr) = m.intermediate_number;
            end
            m.intermediate_number = m.intermediate_number + 1;
        end
        
        function update(m, lhs, value, whenstr, condition)
            % UPDATE - adds an update equation to a state
            % 
            % Usage:
            %   model.UPDATE(state, equation, 'when', condition)
            % 
            % Description:
            %   updates the state expression <state> with the expression
            %   <equation> when the <condition> is true.  This can be used
            %   to define a reset condition for a state variable.
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            if 5 ~= nargin
                error('Simatra:Model:update', 'Incorrect number of input arguments');
            end
            
            % check input correctness
            if ~isa(lhs, 'Exp')
                error('Simatra:Model:update', 'State variable must be an expression type');
            elseif ~isa(value, 'Exp') && ~isnumeric(value)
                error('Simatra:Model:update', 'Value assigned in update must be an expression type');
            elseif ~isa(condition, 'Exp') && ~isnumeric(condition)
                error('Simatra:Model:update', 'Condition tested in update must be an expression type');
            elseif ~strcmpi(whenstr, 'when')
                error('Simatra:Model:update', 'Incorrect usage - see help Model/update for info');
            elseif ~m.States.isKey(toStr(lhs))
                error('Simatra:Model:update', 'Quantity %s is not a state variable', toStr(lhs));
            end
            % make sure these are Expressions just in case they are defined
            % as numeric types
            value = Exp(value);
            condition = Exp(condition);
            
            % add as an intermediate (but don't add to the list of
            % intermediate names)
            m.IntermediateEqs(m.intermediate_number) = struct('lhs', lhs, 'value', value, 'condition', condition);
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

        function recurrenceequ(m, lhs, rhs)
            id = toStr(lhs);
            iter = toIterReference(lhs);
            if isfield(m.RecurrenceEqs, id)
                error('Simatra:Model', ['Recurrence Equation assigning ' lhs ' already exists']);
            elseif isa(iter, 'IteratorReference')
              iter_id = iter.iterator.id;
              if iter.delay == 1
                m.RecurrenceEqs.(id) = struct('lhs', lhs, 'rhs', ...
                                              rhs);    
              else
                error('Simatra:Model:recurrenceequ', ['Can only ' ...
                                    'accommodate iterators expressed '...
                                    'as ' iter_id '+1']);
              end
            elseif m.States.isKey(id) && isa(m.States(id).iterator, ...
                                             'Iterator')
              next_time = m.States(id).iterator+1;
              m.RecurrenceEqs.(id) = struct('lhs', ...
                                            lhs(next_time), ...
                                            'rhs', rhs);              
            else
              error('Simatra:Model:recurrenceequ', ['No iterator was '...
                    'specified for the state'])
            end
        end        

        function t = time(m)
            t = Exp(m.DefaultIterator);
        end
        
        function set.solver(m, varargin)
            if iscell(varargin{1})
                m.solver = varargin{1};
            else
                m.solver = varargin;
            end
            update_iterator(m);
        end
        
        function map = findIterators(m)
            map = containers.Map;
            % Search for iterators everywhere
            %  - first in intermediate equations
            structs = values(m.IntermediateEqs);
            for i=1:length(structs)
                if isfield(structs{i}, 'rhs')
                    rhs = structs{i}.rhs;
                    if isa(rhs, 'Exp')
                        lhs = structs{i}.lhs;
                        map = findIterators(rhs, map);
                    end
                end
                if isfield(structs{i}, 'value')
                    value = structs{i}.value;
                    if isa(value, 'Exp')
                        map = findIterators(value, map);
                    end                    
                end
                if isfield(structs{i}, 'condition')
                    condition = structs{i}.condition;
                    if isa(condition, 'Exp')
                        map = findIterators(condition, map);
                    end                    
                end
            end
            %  - Next in differential equations
            ids = fieldnames(m.DiffEqs);
            for i=1:length(ids)
                rhs = m.DiffEqs.(ids{i}).rhs;
                if isa(rhs, 'Exp')
                    map = findIterators(rhs, map);
                end
            end
            %  - Next in recurrence equations
            ids = fieldnames(m.RecurrenceEqs);
            for i=1:length(ids)
                lhs = m.RecurrenceEqs.(ids{i}).lhs;
                if isa(lhs, 'Exp')
                    map = findIterators(lhs, map);
                end
                rhs = m.RecurrenceEqs.(ids{i}).rhs;
                if isa(rhs, 'Exp')
                    map = findIterators(rhs, map);
                end
            end
            %  - Next in outputs
            structs = values(m.Outputs);
            for i=1:length(structs)
                contents = structs{i}.contents;
                condition = structs{i}.condition;
                for j=1:length(contents)
                    if isa(contents{j}, 'Exp')
                        map = findIterators(contents{j}, map);
                    end
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
            % - Search through randoms
            structs = values(m.Randoms);
            for i=1:length(structs)
                iter = structs{i}.iterator;
                if isa(iter, 'Iterator')
                    map(iter.id) = iter;
                end
            end
            % ADD INPUTS WHEN READY
        end

        function [inputs, outputs] = interface(m)
            inputs = keys(m.Inputs);
            outputs = keys(m.Outputs);
        end
        
        function str = toStr(m)
            inputs = keys(m.Inputs);
            outputs = keys(m.Outputs);
            states = keys(m.States);
            randoms = keys(m.Randoms);
            eqs = keys(m.IntermediateEqs);
            eqsNames = keys(m.IntermediateEqsNames);
            diffeqs = fieldnames(m.DiffEqs); 
            recurrenceeqs = fieldnames(m.RecurrenceEqs);
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
            default_id = m.DefaultIterator.id;
            defined = false;
            for i=1:length(iterators)
                if strcmp(iterators{i}.id, default_id);
                    defined = true;
                end
                str = [str '   ' iterators{i}.toStr '\n'];
            end
            if ~defined
                str = [str '   ' m.DefaultIterator.toStr '\n'];
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
                str = [str '   state ' states{i} ' = ' toStr(state.init) iter_str '\n'];
            end
            str = [str '\n'];
            str = [str '   // Random definitions\n'];
            for i=1:length(randoms)
                state = m.Randoms(randoms{i});
                iter_str = '';
                if isa(state.iterator, 'Iterator')
                    iter_str = ['iter=' state.iterator.id ', '];
                end
                if state.uniform
                  parameter_str = sprintf('uniform, high=%g, low=%g', state.high, ...
                                          state.low);
                else
                  parameter_str = sprintf('normal, mean=%g, stddev=%g', ...
                                          state.mean, ...
                                          state.stddev);
                end
                str = [str '   random ' randoms{i} ' with {' iter_str ...
                       parameter_str '}\n'];
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
                if isfield(equ, 'rhs')
                    rhs = equ.rhs;
                    if isRef(lhs)
                        str = [str '   ' toStr(lhs) ' = ' toStr(rhs) '\n'];
                    else
                        str = [str '   equation ' toStr(lhs) ' = ' toStr(rhs) '\n'];
                    end
                elseif isfield(equ, 'value') && isfield(equ, 'condition')
                    value = equ.value;
                    condition = equ.condition;
                    str = [str '   equation ' toStr(lhs) ' = ' toStr(value) ' when ' toStr(condition) '\n'];
                else
                    error('Simatra:Model:toStr', 'Unexpected equation form');
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
            str = [str '   // Recurrence equation definitions\n'];
            str = [str '   equations\n'];
            for i=1:length(recurrenceeqs)
              key = recurrenceeqs{i};
                lhs = m.RecurrenceEqs.(key).lhs;
                rhs = m.RecurrenceEqs.(key).rhs;
                str = [str '      ' toStr(lhs) ' = ' toStr(rhs) '\n'];
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
        
        
        function filename = toDSL(m, filename)
            disp(['Creating ' m.Name ' ...']);
            str = toStr(m);
            % if one is not specified, then create one
            if nargin == 1
                filename = fullfile(tempdir, [m.Name '.dsl']);
            end
            
            writeFile = true;
            if exist(filename, 'file')
                str_from_file = fileread(filename);
                str_from_file = regexprep(str_from_file, char(10), '\\n');
                str_from_file = regexprep(str_from_file, char(37), '%%');
                if strcmp(str, str_from_file)
                    disp(['Reusing file ' filename]);
                    writeFile = false;
                else
%                     disp(sprintf('files do not match (length: %d != %d)', length(str), length(str_from_file)));
%                     disp('=================================================================');
%                     str
%                     disp('-----------------------------------------------------------------');
%                     str_from_file
%                     disp('=================================================================');
                end
            else
%                 disp(sprintf('file %s does not exist', filename));
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
        
        % overload the simex function
        function varargout = simex(m, varargin)
            % MODEL/SIMEX - execute a Model simulation
            % 
            % Usage:
            %   [outputs] = SIMEX(MDL, args) - simulate and return
            %   outputs as a structure
            %
            %   [outputs, final_states] = SIMEX(MDL, args) -
            %   additionally return the final states
            %
            %   [outputs, final_states, final_time] = SIMEX(MDL,
            %   args) - additionally return the final_time
            %
            % Input argument MDL is the Model object generated. All
            % other arguments are described in the help for SIMEX. 
            %
            % See also SIMEX
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            %
            
            % execute the simulation
            varargout = cell(1,nargout);
            if nargout > 0
                [varargout{:}] = simex(toDSL(m), varargin{:}, '-fastcompile');
            else
                varargout{1} = simex(toDSL(m), varargin{:}, '-fastcompile');
            end
        end
        
    end
    
    methods (Access = protected)
        function order_equations(m)
            equkeys = m.IntermediateEqs.keys;
            
            % create dependency list
            dependencies = cell(1,length(equkeys));
            all_lhs = {};
            for i=1:length(equkeys)
                s = m.IntermediateEqs(equkeys{i});
                lhs = s.lhs;
                rhs = s.rhs;
                lhs_symbols = exp_to_symbols(Exp(lhs));
                all_lhs = [all_lhs lhs_symbols];
                rhs_symbols = exp_to_symbols(Exp(rhs));
                deps = {lhs_symbols, rhs_symbols, s};
                dependencies{i} = deps;
            end
            
            % prune rhs for all dependencies that don't appear on the lhs
            for i=1:length(dependencies)
                deps = dependencies{i};
                rhs = deps{2};
                new_rhs = {};
                for j=1:length(rhs)
                    if any(strcmp(all_lhs, rhs{j}))
                        new_rhs{end+1} = rhs{j};
                    end
                end
                deps{2} = new_rhs;
                dependencies{i} = deps;
            end
            
            % create a new list that will be populated
            map = containers.Map(1, struct());
            map.remove(1);
            
            function syms = get_cleared_deps()
                keys = map.keys;
                syms = {};
                for i=1:length(keys)
                    s = map(keys{i});
                    syms = [syms exp_to_symbols(Exp(s.lhs))];
                end
            end
            count = 1;
            while(~isempty(dependencies))
                cleared_deps = get_cleared_deps();
                remaining_deps = {};
                for i=1:length(dependencies)
                    % if all the dependencies are clear then it is good
                    passed = true;
                    deps = dependencies{i};
                    rhs_symbols = deps{2};
                    for j=1:length(rhs_symbols)
                        if ~any(strcmp(cleared_deps, rhs_symbols{j}))
                            % the symbol does not yet appear
                            passed = false;
                            break;
                        end
                    end
                    if passed
                        map(count) = deps{3};
                        count = count + 1;
                    else
                        remaining_deps{end+1} = dependencies{i};
                    end
                   
                end
                dependencies = remaining_deps;
            end
            
            % reassign the new reordered equations
            m.IntermediateEqs = map;
        end
        
        function update_iterator(m)
            iter_id = m.DefaultIterator.id;
            iter = Iterator(iter_id, 'solver', m.solver{:});
            m.DefaultIterator = iter;
        end
        
        function r = identifier_exists(m, id)
            r = m.Inputs.isKey(id) || ...
                m.States.isKey(id) || ...
                m.Instances.isKey(id) || ...
                m.IntermediateEqsNames.isKey(id);
        end
        
        function initializeModel(m)
            m.Inputs = containers.Map;
            m.Outputs = containers.Map;
            m.States = containers.Map;
            m.Randoms = containers.Map;
            m.Instances = containers.Map;
            m.IntermediateEqs = containers.Map(1, struct());
            m.IntermediateEqs.remove(1);
            m.DiffEqs = struct();
            m.RecurrenceEqs = struct();
            m.cachedModels = containers.Map;
            m.instance_number = 1;
            m.state_number = 1;
            m.intermediate_number = uint32(1);
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

function s = toStr(r)
if isnumeric(r)
    s = num2str(r);
elseif isstruct(r)
    disp('Simatra:Model:toStr', 'Unexpected structure found');
elseif isstr(r)
    s = r;
else
    s = r.toStr;
end
end


