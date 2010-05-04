classdef Model < handle
   
    properties (SetAccess = private)
        Name
        Inputs
        Outputs
        States
        Instances
        IntermediateEqs
        DiffEqs
    end
    
    properties (Access = private)
        instance_number = 1
    end
    
    methods
        function m = Model(Name)
            m.Name = Name;
            m.Inputs = struct();
            m.Outputs = struct();
            m.States = struct();
            m.Instances = struct();
            m.IntermediateEqs = struct();
            m.DiffEqs = struct();
        end
        
        function e = addState(m, id, init)
            if isfield(m, id)
                error('Simatra:Model', ['State ' id ' already exists']);
            else
                e = Exp(id);
                m.States.(id) = struct('init', init);
            end
        end
        
        function e = addInput(m, id, default)
            if isfield(m, id)
                error('Simatra:Model', ['Input ' id ' already exists']);
            else
                e = Exp(id);
                if nargin == 3
                    m.Inputs.(id) = struct('default', default);
                else
                    m.Inputs.(id) = struct();
                end
            end
        end
        
        function addOutput(m, exp)
            id = toStr(exp);
            if isfield(m, id)
                error('Simatra:Model', ['Output ' id ' already exists']);
            else
                e = Exp(id);
                m.Outputs.(id) = struct();    
            end
        end
        
        function instance = addInstance(m, modelarg)
            if ischar(modelarg) && exists(modelarg, 'file')
                minfo = simex(modelarg);
                name = minfo.name;
                inputs = minfo.inputs;
                outputs = minfo.outputs;
            elseif isa(modelarg,'Model')
                name = modelarg.Name;
                inputs = fieldnames(modelarg.Inputs);
                outputs = fieldnames(modelarg.Outputs);                
            else
                error('Simatra:Model', ['Unexpected instance'])
            end
            inst = ['Instance_' num2str(m.instance_number)];
            m.instance_number = m.instance_number + 1;
            m.Instances.(inst) = struct('name', name, 'inputs', inputs, 'outputs', outputs, 'obj', modelarg);
            instance = Instance(inst, m, inputs, outputs);
        end

        function e = addEq(m, lhs, rhs)
            if isa(lhs, 'Exp')
                lhsstr = toId(lhs);
            else
                lhsstr = lhs;
                lhs = Exp(lhs);
            end
            if isfield(m, lhsstr)
                error('Simatra:Model', ['Equation assigning ' lhs ' already exists']);
            else
                e = Exp(lhsstr);
                m.IntermediateEqs.(lhsstr) = struct('lhs', lhs, 'rhs', rhs);
            end
        end
        
        function addDiffEq(m, lhs, rhs)
            id = toStr(lhs);
            if isfield(m, id)
                error('Simatra:Model', ['Differential Equation assigning ' lhs ' already exists']);
            else
                m.DiffEqs.(id) = struct('lhs', lhs, 'rhs', rhs);    
            end
        end
        
%        function x = subsref(m, field)
%            x = Exp(1);
%        end
        


        function str = toStr(m)
            inputs = fieldnames(m.Inputs);
            outputs = fieldnames(m.Outputs);
            states = fieldnames(m.States);
            eqs = fieldnames(m.IntermediateEqs);
            diffeqs = fieldnames(m.DiffEqs);
            instances = fieldnames(m.Instances);

            str = '';
            str = [str '// Generated DSL model: ' m.Name '\n'];
            str = [str '// Created: ' datestr(now) '\n'];
            str = [str '// Copyright 2010 Simatra Modeling Technologies\n'];
            str = [str '\n'];
            str = [str '// Import List\n'];
            for i=1:length(instances)
                obj = m.Instances.(instances{i}).obj;
                if ischar(obj)
                    str = [str 'import "' obj '"' '\n'];
                else
                    str = [str toStr(obj) '\n'];
                end
            end
            str = [str '\n'];                
            outputList = ['(' concatWith(', ', outputs) ')'];
            inputList = ['(' concatWith(', ', inputs), ')'];
            str = [str 'model ' outputList ' = ' m.Name inputList '\n'];
            str = [str '\n'];
            str = [str '   // Input definitions\n'];
            for i=1:length(inputs)
                if isfield(m.Inputs.(inputs{i}), 'default')
                    attributes = [' with {default=' num2str(m.Inputs.(inputs{i}).default) '}'];
                else
                    attributes = '';
                end
                str = [str '   input ' inputs{i} attributes '\n'];
            end
            str = [str '\n'];
            str = [str '   // State definitions\n'];
            for i=1:length(states)
                str = [str '   state ' states{i} ' = ' num2str(m.States.(states{i}).init) '\n'];
            end
            str = [str '\n'];
            str = [str '   // Instance definitions\n'];
            for i=1:length(instances)
                name = m.Instances.(instances{i}).name;
                str = [str '   submodel ' name ' ' instances{i} '\n'];
            end
            str = [str '\n'];
            str = [str '   // Equation definitions\n'];
            for i=1:length(eqs)
                lhsstr = eqs{i};
                lhs = m.IntermediateEqs.(lhsstr).lhs;
                rhs = m.IntermediateEqs.(lhsstr).rhs;
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
                str = [str '   output ' outputs{i} '\n'];
            end
            str = [str 'end\n'];
        end
        
        
        function filename = toDSL(m)
            str = toStr(m);            
            filename = fullfile(tempdir, [m.Name '.dsl']);
            fid = fopen(filename, 'w');
            fprintf(fid, str);
            fclose(fid);
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
