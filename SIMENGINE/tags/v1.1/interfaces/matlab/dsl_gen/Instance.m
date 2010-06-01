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
                items = strfind(inst.Outputs, s.subs);
                if isempty(cell2mat(items))
                    inst.Outputs
                    error('Simatra:Instance', ['No output with name ' s.subs]);
                end
                output = Exp(inst.InstName, s.subs);
            end
            b = output;
        end
        
        function i2 = subsasgn(inst, s, b)
            if length(s) == 1 && isfield(s, 'type') && strcmp(s.type, '.')
                input = Exp(inst.InstName, s.subs);
                inst.Mdl.addEq(input, b);
            end
            i2 = inst;
        end
    end
    
end