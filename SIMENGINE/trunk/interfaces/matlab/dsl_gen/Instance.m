classdef Instance < handle
    
    properties
        MdlName
        InstName
        ClassName
        Inputs
        Outputs
        Mdl
        SubMdl
        Dims
        MainInst
        Derived
        Indices
    end
    
    properties (Access = protected)
        definedInputs
        definedIndices
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
            inst.Derived = false;
            inst.Indices = reshape(1:prod(Dims), Dims);
            inst.definedIndices = zeros(Dims);
        end
        
        function b = subsref(inst, s)
            nextsub = 2;
            if strcmp(s(1).type, '.')
                out = s(1).subs;
                if strcmp(out, 'toStr')
                    b = toStr(inst);
                % Return any properties that are referenced
                elseif any(strcmp(out, fieldnames(inst)))
                    b = inst.(out);
                elseif isOutput(inst, out)
                  if inst.Derived
                    b = Exp(inst, out);
                    b = reshape(b(inst.Indices), inst.Dims);
                  else
                    b = Exp(inst, out);
                  end
                elseif isInput(inst, out)
                    % now, if it's an input, we can still handle that by
                    % returning what ever is defined, if it has been
                    % defined
                    if inst.derived
                      maininst = inst.MainInst;
                    else
                      maininst = inst;
                    end
                    if isKey(maininst.definedInputs, out) && isequal(maininst.definedIndices, ones(maininst.Dims))
                        b = maininst.definedInputs(out);
                    else
                        error('Simatra:Instance', ['Can not read from an input ''%s'' of submodel ''%s'' if it has not been already defined.'], out, inst.SubMdl.Name)
                    end    
                elseif length(s) >= 2 && strcmp(s(1).subs, 'with') && strcmp(s(2).type, '()')
                    b = with(inst, s(2).subs);
                    nextsub = 3;
                else
                    inst.Outputs
                    error('Simatra:Instance', 'No output with name %s found', s.subs);
                end
            elseif strcmp(s(1).type, '()')
              if length(s) >= 2 && strcmp(s(2).type, '.')
                news = s;
                news([1 2]) = s([2 1]); % flip first and second subsref
                b = subsref(inst, news);
                nextsub = 3;
              else
                % Create a derived Instance that is a subset of the original instance
                b = Instance(inst.MdlName, inst.InstName, inst.Mdl, inst.SubMdl, inst.Inputs, inst.Outputs, inst.Dims);
                b.Derived = true;
                b.MainInst = inst;
                b.Indices = subsref(inst.Indices, s);
                b.Dims = size(b.Indices);
              end
            else
                for i=1:length(s)
                    i
                    s(i)
                end
                error('Simatra:Instance', 'Unexpected argument syntax');
            end
            if length(s) >= nextsub
              b = subsref(b, s(nextsub:end));
            end
        end
        
        function i2 = subsasgn(inst, s, b)
            if length(s) == 1
              if strcmp(s.type, '.')
                inp = s.subs;
                if inst.Derived
                  s = struct('type', '()', 'subs', inst.Indices);
                  setInput(inst.MainInst, inp, b, s);
                else
                  setInput(inst, inp, b);
                end
              else
                error('Simatra:Instance:subsasgn', 'Inputs of instances must be assigned as Inst.inp, Inst(subs).inp or Inst.inp(subs)');
              end
            elseif length(s) == 2
              if strcmp(s(1).type, '.') && strcmp(s(2).type, '()')
                %error('Simatra:Instance:subsasgn', 'Not yet implemented inst.out(subs) = value.');
                setInput(inst, s(1).subs, b, s(2));
              elseif strcmp(s(1).type, '()') && strcmp(s(2).type, '.')
                %error('Simatra:Instance:subsasgn', 'Not yet implemented inst(subs).out = value.');
                setInput(inst, s(2).subs, b, s(1));
              else
                error('Simatra:Instance:subsasgn', 'Inputs of instances must be assigned as Inst.inp, Inst(subs).inp or Inst.inp(subs)');
              end
            else
              error('Simatra:Instance:subsasgn', 'Inputs of instances must be assigned as Inst.inp, Inst(subs).inp or Inst.inp(subs)');
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
                error('Requested dimension from size is not a scalar numeric value.');
              elseif dim > length(inst.Dims)
                d = 1;
              else
                d = inst.Dims(dim);
              end
            else
              error(['Wrong number of arguments to Instance/size(). Expected 1 or 2 but got ' num2str(nargin) '.']);
            end
        end
        
        function d = length(inst)
            d = max(size(inst));
        end
        
        function setInput(inst, inp, value, subs)
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
            input = Exp(inst, inp);
            if nargin == 4
              input = subsref(input, subs);
            end
            inst.Mdl.equ(input, value);
            if nargin == 4
              if isKey(inst.definedInputs, inp)
                oldvalue = inst.definedInputs(inp);
              else
                oldvalue = Exp('NULLEXP', inst.Dims);
              end
              oldvalue(subs) = value;
              value = oldvalue;
              newIndices = zeros(inst.Dims);
              newIndices(subs) = 1;
              inst.definedIndices = inst.definedIndices | newIndices;
            end
            inst.definedInputs(inp) = value;
        end
        
        function b = with(inst, inputValues)
            if isempty(inputValues)
              error('Simatra:Instance:with', 'No arguments. Setting inputs using .with() requires .with(''input'', value [,''input2'', value2 [...]])');
            end
            if mod(length(inputValues),2) ~= 0
              error('Simatra:Instance:with', 'Odd number of arguments. Setting inputs using .with() requires .with(''input'', value [,''input2'', value2 [, ...]])');
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
          if inst.Derived
            error('Simatra:Instance:toStr', 'Subsrefernced submodels should never be written to strings.');
          end
          str = '';
          numInst = prod(inst.Dims);
          if numInst > 1
            for i = 1:numInst;
              str = [str '    submodel ' inst.MdlName ' ' inst.InstName '_' num2str(i) '\n'];
            end
          else
            str = ['    submodel ' inst.MdlName ' ' inst.InstName '\n'];
          end
        end
    end
    
end