classdef Iterator < handle
% Iterator - Define a temporal iterator for use in a simEngine Model
%
% Model Methods
%   Constructor:
%   Iterator - create a new iterator object
%
% Copyright 2010 Simatra Modeling Technologies
% Website: www.simatratechnologies.com
% Support: support@simatratechnologies.com
%
    
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
    
    properties (GetAccess = public, SetAccess = private)
        solvers = {'forwardeuler', 'linearbackwardeuler', 'exponentialeuler', 'heun', 'rk4', 'cvode', 'ode23', 'ode45'};
    end
    
    methods
        function iter = Iterator(varargin)
            % ITERATOR - create a new Iterator object
            %
            % Usage:
            %   t = Iterator([ID,] 'continuous', 'solver', SOLVER [, PARAMETERS])
            %   - create a continuous time iterator with a specified
            %   solver.  The supported solvers are listed below.
            %
            %   n = Iterator([ID,] 'discrete', 'sample_period', T) -
            %   create a discrete time iterator with a specified period.
            %   The update rate also be expressed as a frequency with an alternative
            %   'sample_frequency' keyword.
            %
            % Options:
            %   SOLVER can be one of:
            %   'forwardeuler' - 1st-order explicit Euler method
            %   'heun' - 2nd-order predictor corrector method
            %   'rk4' - 4th-order explicit Runga-Kutta method
            %   'ode23' - 2nd-order variable time step solver using
            %   Bogacki-Shampine (2,3) pair
            %   'ode45' - 4th-order variable time step solver using
            %   Dormand-Prince (4,5) pair
            %   'exponentialeuler' - explicit 1st-order method used on problems with
            %   a stiff linear term
            %   'linearbackwardeuler' - 1st-order backward Euler scheme
            %   usable on states that are linear in relationship to each
            %   other.
            %   'cvode' - calls the CVode solver developed under SUNDIALS.
            %
            % Examples:
            %   t_implicit = Iterator('continuous', 'solver',
            %   'linearbackwardeuler', 'dt', 0.01);
            %   t_cvode = Iterator('continuous', 'solver', 'cvode');
            %   n = Iterator('discrete', 'sample_frequency', 8192);
            %
            % See also MODEL/MODEL MODEL/STATE MODEL/INPUT MODEL/RANDOM
            % MODEL/OUTPUT
            %
            % Copyright 2010 Simatra Modeling Technologies
            % Website: www.simatratechnologies.com
            % Support: support@simatratechnologies.com
            
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
                        switch args{2}
                            case iter.solvers
                                iter.solver = args{2};
                            otherwise
                                error('Simatra:Iterator', 'Invalid solver specified.  Options are: %s', List.stringConcatWith(', ', iter.solvers));
                        end
                        args = args(3:end);
                    else
                        error('no solver');
                    end
                elseif strcmpi(arg, 'dt') || strcmpi(arg, 'sample_period') || strcmpi(arg, 'sample_frequency')
                    if length(args) > 1
                        if isnumeric(args{2})
                            if strcmpi(arg, 'sample_frequency')
                                iter.dt = 1/args{2};
                            else
                                iter.dt = args{2};
                            end
                            args = args(3:end);
                        else
                            error('Simatra:Iterator', 'non-numeric dt')
                        end
                    else
                        error('Simatra:Iterator', 'no dt');
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