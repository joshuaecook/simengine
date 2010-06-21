classdef Iterator < handle
   
    properties
        id
        type
        solver
        dt
    end
    
    properties
        iterator_count = 0
        timestamp
    end
    
    methods
        function iter = Iterator(varargin)
            iter.timestamp = now;
            % set defaults
            iter.type = 'continuous';
            [filepath, filename, fileext] = fileparts(tempname);
            iter.id = ['iterator_' filename];
            iter.solver = 'ode45';
            iter.dt = 1;
            args = varargin;
            i = 1;
            while (~isempty(args))
                arg = args{1};
                if strcmpi(arg, 'continuous')
                    iter.type = 'continuous';
                    args = args(2:end);
                elseif strcmpi(arg, 'discrete')
                    iter.type = 'discrete';
                    args = args(2:end);
                elseif strcmpi(arg, 'solver')
                    if length(args) > 1
                        iter.solver = args{2};
                        args = args(3:end);
                    else
                        error('no solver');
                    end
                elseif strcmpi(arg, 'dt') || strcmpi(arg, 'sample_period')
                    if length(args) > 1
                        if isnumeric(args{2})
                            iter.dt = args{2};
                            args = args(3:end);
                        else
                            error('non-numeric dt')
                        end
                    else
                        error('no dt');
                    end
                elseif 1 == i && ischar(arg)
                    iter.id = arg;
                    args = args(2:end);
                else 
                  if ischar(arg)
                    error('Simatra:Iterator', ['Unexpected argument ' ...
                    '%s'], arg);
                  elseif isnumeric(arg)
                    error('Simatra:Iterator', ['Unexpected argument ' ...
                    '%g'], arg);
                  end
                end
                i = i + 1;
            end
            
        end
        
        function r = isDiscrete(iter)
            r = strcmpi(iter.type, 'discrete');
        end
        function r = isContinuous(iter)
            r = strcmpi(iter.type, 'continuous');
        end
        function iterref = toReference(iter)
            iterref = IteratorReference(iter, 0);
        end
        
        function iterref = plus(iter, val)
            iterref = IteratorReference(iter, val);
        end
        function iterref = minus(iter, val)
            iterref = IteratorReference(iter, -val);
        end
        
        function str = toStr(iter)
            if strcmpi(iter.type, 'discrete')
                options = ['sample_period=' num2str(iter.dt)];
            else
                options = ['solver=' iter.solver '{dt=' num2str(iter.dt) '}'];
            end
            str = ['iterator ' iter.id ' with {' iter.type ', ' options '}'];
        end
        
        function disp(iter)
            if strcmpi(iter.type, 'discrete')
                disp(['Discrete iterator ' iter.id ' with timestep = ' num2str(iter.dt)])
            else
                disp(['Continuous iterator ' iter.id ' with solver = ' iter.solver])
            end
        end            
    end
    
end