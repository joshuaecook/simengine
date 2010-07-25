classdef Instance
    
    properties
        InstName
        ClassName
        Inputs
        Outputs
        Mdl
    end
    
    methods
        function inst = Instance(InstName, Mdl, Inputs, Outputs)
            inst.Mdl = Mdl;
            inst.InstName = InstName;
            inst.Inputs = Inputs;
            inst.Outputs = Outputs;
        end
        
        function b = subsref(inst, s)
            if length(s) == 1 && isfield(s, 'type') && strcmp(s.type, '.')
                % make sure that the output exists
                out = s.subs;
                %items = strfind(inst.Outputs, s.subs);
                %if isempty(cell2mat(items))
                if isOutput(inst, out)
                    output = Exp(inst.InstName, s.subs);
                    b = output;
                elseif strcmp(out, 'Inputs')
                    b = inst.Inputs;
                elseif strcmp(out, 'Outputs')
                    b = inst.Outputs;
                else
                    inst.Outputs
                    error('Simatra:Instance', 'No output with name %s found', s.subs);
                end
            else
                for i=1:length(s)
                    i
                    s(i)
                end
                error('Simatra:Instance', 'Unexpected argument syntax');
            end
        end
        
        function i2 = subsasgn(inst, s, b)
            if length(s) == 1 && isfield(s, 'type') && strcmp(s.type, '.')
                inp = s.subs;
                if ~isInput(inst, inp)
                    inst.Inputs
                    error('Simatra:Instance', 'No input with name %s found', inp);
                end
                input = Exp(inst.InstName, s.subs);
                inst.Mdl.equ(input, b);
            end
            i2 = inst;
        end
        
        function r = isInput(inst, inp)
            r = any(strcmp(inst.Inputs, inp));
        end
        
        function r = isOutput(inst, out)
            r = any(strcmp(inst.Outputs, out));
        end
    end
    
end