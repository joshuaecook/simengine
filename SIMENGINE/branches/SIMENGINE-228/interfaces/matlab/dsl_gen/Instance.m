classdef Instance
    
    properties
        MdlName
        InstName
        ClassName
        Inputs
        Outputs
        Mdl
        SubMdl
        Dims
        NumInst
    end
    
    properties (Access = protected)
        definedInputs
    end
    
    methods
        function inst = Instance(MdlName, InstName, Mdl, SubMdl, Inputs, Outputs, Dims)
            inst.MdlName = MdlName;
            inst.Mdl = Mdl;
            inst.SubMdl = SubMdl;
            inst.InstName = InstName;
            inst.Inputs = Inputs;
            inst.Outputs = Outputs;
            inst.definedInputs = containers.Map;
            inst.Dims = Dims;
            inst.NumInst = prod(Dims);
        end
        
        function b = subsref(inst, s)
            if length(s) == 1 && isfield(s, 'type') && strcmp(s.type, '.')
                out = s.subs;
                if strcmp(out, 'toStr')
                    b = toStr(inst);
                % Return any properties that are referenced
                elseif any(strcmp(s.subs, fieldnames(inst)))
                    b = inst.(s.subs);
                elseif isOutput(inst, out)
                    if inst.NumInst > 1
                      output = cell(1, inst.NumInst); %cell(inst.Dims);
                      for i = 1:inst.NumInst
                        output{i} = Exp([inst.InstName '_' num2str(i)], out);
                      end
                    else
                      output = Exp(inst.InstName, out);
                    end
                    b = output;
                elseif isInput(inst, out)
                    % now, if it's an input, we can still handle that by
                    % returning what ever is defined, if it has been
                    % defined
                    if isKey(inst.definedInputs, out)
                        b = inst.definedInputs(out);
                    else
                        error('Simatra:Instance', ['Can not read from an input ''%s'' of submodel ''%s'' if it has not been already defined.'], out, inst.SubMdl.Name)
                    end    
                else
                    inst.Outputs
                    error('Simatra:Instance', 'No output with name %s found', s.subs);
                end
            %elseif length(s) == 2 && isfield(s(1), 'type') && ...
            %      strcmp(s(1).type, '()') && isfield(s(2), 'type') ...
            %      && strcmp(s(2).type, '.'
            %  out = s(2).subs;
            elseif length(s) == 2 && isfield(s(1), 'type') && ...
                  strcmp(s(1).type, '.') && isfield(s(2), 'type') && ...
                  strcmp(s(2).type, '()') && strcmp(s(1).subs, 'with')
                b = with(inst, s(2).subs);
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
                setInput(inst, inp, b);
            end
            i2 = inst;
        end
        
        function d = size(inst, dim)
            if nargin == 1
              if isscalar(inst.Dims)
                d = [1 inst.Dims];
              else
                d = inst.Dims;
              end
            elseif nargin == 2
              if ~isnumeric(dim) || ~isscalar(dim)
                error(['Requested dimension from size is not a scalar ' ...
                       'numeric value.']);
              elseif dim > length(inst.Dims)
                d = 1;
              else
                d = inst.Dims(dim);
              end
            else
              error(['Wrong number of arguments to Instance/size(). ' ...
                     'Expected 1 or 2 but got ' num2str(nargin) '.']);
            end
        end
        
        function d = length(inst)
            d = max(size(inst));
        end
        
        %function d = numel(inst, varargin)
        %    if nargin == 1
        %      d = inst.NumInst;
        %    else
        %      d = numel([varargin{:}]);
        %    end
        %end
        
        function setInput(inst, inp, value)
            if ~isa(inst, 'Instance')
              error('Simatra:Instance', 'setInput requires an Instance object.');
            end
            if ~ischar(inp)
              error('Simatra:Instance', 'setInput requires inp to be a string name of an input.');
            end
            if ~isInput(inst, inp)
              inst.Inputs
              error('Simatra:Instance', 'No input with name %s found', inp);
            end
            input = Exp(inst.InstName, inp);
            inst.Mdl.equ(input, value);
            inst.definedInputs(inp) = value;
        end
        
        function b = with(inst, inputValues)
            if isempty(inputValues)
              error('Simatra:Instance', 'No arguments. Setting inputs using .with() requires .with(''input'', value [,''input2'', value2 [...]])');
            end
            if mod(length(inputValues),2) ~= 0
              error('Simatra:Instance', 'Odd number of arguments. Setting inputs using .with() requires .with(''input'', value [,''input2'', value2 [, ...]])');
            end
            for arg = 1:2:length(inputValues)
              setInput(inst, inputValues{arg}, inputValues{arg+1});
            end
            b = inst;
        end
        
        function r = isInput(inst, inp)
            r = any(strcmp(inst.Inputs, inp));
        end
        
        function r = isOutput(inst, out)
            r = any(strcmp(inst.Outputs, out));
        end
        
        function str = toStr(inst)
          str = '';
          if inst.NumInst > 1
            for i = 1:inst.NumInst;
              str = [str '    submodel ' inst.MdlName ' ' inst.InstName '_' num2str(i) '\n'];
            end
          else
            str = ['    submodel ' inst.MdlName ' ' inst.InstName '\n'];
          end
        end
    end
    
end