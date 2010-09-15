classdef Iterator < handle
% Iterator - Define a temporal iterator for use in a simEngine Model
%
% Iterator Methods
%   Constructor:
%   Iterator - create a new iterator object
%
% Iterator Properties
%   solver   - string name of the numerical method
%   dt       - time step of the numerical method
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
    
    properties (Access = protected)
        params
    end
    
    properties (Access = private)
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
            
            iter.params = containers.Map;
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
                    
                    % set the default params
                    setDefaultParams(iter);
                    
                    args = args(2:end);
                elseif strcmpi(arg, 'solver')
                    if length(args) > 1
%                         switch args{2}
%                             case iter.solvers
%                                 iter.solver = args{2};
%                             otherwise
%                                 error('Simatra:Iterator', 'Invalid solver specified.  Options are: %s', List.stringConcatWith(', ', iter.solvers));
%                         end
                        iter.solver = args{2};
                        
                        % once we know the solver, we can set the default
                        % parameters
                        setDefaultParams(iter);
                        
                        % continue on processing
                        args = args(3:end);
                    else
                        error('Simatra:Iterator', ['Must follow the ''solver'' keyword with the name of one of the supported solvers: ' List.cell2str(iter.solvers)]);
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
        
        function set.solver(iter, solv)
            if ~ischar(solv)
                error('Simatra:Iterator:solver', 'The value passed for solver must be a string quantity');
            end
            switch solv
                case iter.solvers
                    iter.solver = solv;
                otherwise
                    error('Simatra:Iterator:solver', 'Invalid solver specified.  Options are: %s', List.cell2str(iter.solvers));
            end
        end
        
        function set.dt(iter, dt)
            if ~isnumeric(dt)
                error('Simatra:Iterator:dt', 'The value passed for dt must be a numeric quantity');
            elseif dt <= 0
                error('Simatra:Iterator:dt', 'The value passed for dt must be greater than zero');
            end                
                
            iter.dt = dt;
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
        
        function str = toInfo(iter)
            if strcmpi(iter.type, 'discrete')
                options = ['sample_period=' num2str(iter.dt)];
                str = sprintf('Discrete iterator with {%s}', options);
            else
                options = ['solver=' iter.solver '{dt=' num2str(iter.dt) '}'];
                str = sprintf('Continuous iterator with {%s}', options);
            end
        end
        
        function disp(iter)
            if strcmpi(iter.type, 'discrete')
                disp(['Discrete iterator ' iter.id ' with timestep = ' num2str(iter.dt)])
            else
                disp(['Continuous iterator ' iter.id ' with solver = ' iter.solver])
            end
        end
    
%         function [varargout] = properties(iter)
%             % Determine all the properties
%             props = keys(iter.params);
%             if isDiscrete(iter)
%                 p = cell(length(props),1);
%             else
%                 p = cell(length(props)+1,1);
%             end
%             p(1:length(props)) = props;
%             if isContinuous(iter)
%                 p{end} = 'solver';
%             end
%             
%             % return the output
%             if nargout == 0
%                 disp(' ');
%                 disp('Properties for class Iterator:')
%                 disp(' ');
%                 for i=1:length(p)
%                     disp(['    ' p{i}]);
%                 end
%                 disp(' ');
%             else
%                 varargout{1} = p;
%             end
%         end
        
        function varargout = subsref(m, args)
            varargout = cell(1,nargout);
            for i=1:length(args)
                switch args(i).type
                    case '{}'
                        error('unexpected {} type');
                    case '()'
                        error('unexpected () type');
                    case '.'
                        %disp(sprintf('Finding %s', args(i).subs));
                        if any(strcmp(methods(m), args(i).subs))
                            if nargout > 0
                                [varargout{:}] = feval(args(i).subs, m, args(i+1).subs{:});
                            else
                                feval(args(i).subs, m, args(i+1).subs{:});
                            end
                        elseif any(strcmp(keys(m.params), args(i).subs))
                            p = m.params(args(i).subs);
                            varargout{1} = p;
                        elseif any(strcmp(properties(m), args(i).subs))
                            varargout{1} = m.(args(i).subs);
                        else
                            warning('Simatra:Iterator:subsref', ['There is no property or method with name ''' args(i).subs ''' defined'])
                        end
                        break;
                end
            end
        end
        
        function m = subsasgn(m, args, val)
            for i=1:length(args)
                switch args(i).type
                    case '{}'
                        error('unexpected {} type');
                    case '()'
                        error('unexpected () type');
                    case '.'
                        %disp(sprintf('Assigning %s', args(i).subs));
                        if any(strcmp(methods(m), args(i).subs))
                            error('Simatra:Iterator:subsasgn', 'Can''t assign to method name %s', args(i).subs)
                        elseif any(strcmp(keys(m.params), args(i).subs))
                            m.params(args(i).subs) = val;
                        elseif any(strcmp(properties(m), args(i).subs))
                            m.(args(i).subs) = val;
                        else
                            warning('Simatra:Iterator:subsasgn', ['Don''t recognize ''' args(i).subs ''''])
                        end
                        break;
                end
            end
            
        end
        
    end
    
    
    methods (Access = protected)
        function setDefaultParams(iter)
            if isDiscrete(iter)
                iter.params('sample_period') = 1;
                iter.params('sample_frequency') = 1;
            else % else if is continuous
                switch iter.solver
                    case {'forwardeuler', 'linearbackwardeuler', 'exponentialeuler', 'rk4', 'heun'}
                        iter.params('dt') = 1;
                    case {'ode23', 'ode45'}
                        iter.params('dt') = 1;
                        iter.params('reltol') = 1e-3;
                        iter.params('abstol') = 1e-6;
                    case {'cvode'}
                        iter.params('dt') = 1;
                        iter.params('cv_lmm') = 'CV_BDF';
                        iter.params('cv_iter') = 'CV_NEWTON';
                end
            end
            
        end
    end
end