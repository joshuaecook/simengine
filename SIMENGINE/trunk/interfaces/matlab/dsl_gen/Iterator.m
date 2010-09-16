classdef Iterator < handle
% Iterator - Define a temporal iterator for use in a simEngine Model
%
% Iterator Methods
%   Constructor:
%   Iterator - create a new iterator object
%
% Iterator Properties
%   solver   - string name of the numerical method
%   <options per solver> - each solver or method has its own options
%
% Options:
%   Depending on the particular solver available, you will have more or
%   less options that can be set.  You can view the values of the available
%   options by display the Iterator object on the command line.  Then, the
%   particular option can be set by specifying IteratorObject.<option> =
%   <value>.
%
% Examples
%   % Create an accurate variable time step solver
%   t = Iterator('continuous', 'solver', 'ode45');
%   t.reltol = 1e-8; % set the relative tolerance
%   t.abstol = 1e-8; % set the absolute tolerance
%
% Copyright 2010 Simatra Modeling Technologies
% Website: www.simatratechnologies.com
% Support: support@simatratechnologies.com
%
    
    properties
        id
        type
        solver
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
            %
            %   t_accurate = Iterartor('continuous', 'solver', 'ode45',
            %   'reltol', 1e-8, 'abstol', 1e-8);
            %   t_cvode = Iterator('continuous', 'solver', 'cvode');
            %
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
            %iter.solver = 'ode45';
            %iter.dt = 1;
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
                    if ~isContinuous(iter)
                        error('Simatra:Iterator', 'Can only specify a solver in an iterator when the iterator is continuous.');
                    end
                    if length(args) > 1
                        iter.solver = args{2};
                        
                        % once we know the solver, we can set the default
                        % parameters
                        setDefaultParams(iter);
                        
                        % continue on processing
                        args = args(3:end);
                    else
                        error('Simatra:Iterator', ['Must follow the ''solver'' keyword with the name of one of the supported solvers: ' List.cell2str(iter.solvers)]);
                    end
                elseif isKey(iter.params, arg)
                    %strcmpi(arg, 'dt') || strcmpi(arg, 'sample_period') || strcmpi(arg, 'sample_frequency')
                    if length(args) > 1
                        iter = subsasgn(iter, struct('type', '.', 'subs', arg), args{2});
                    else
                        error('Simatra:Iterator', ['Must follow the ''' arg ''' keyword with a property value']);
                    end
                    args = args(3:end);
                elseif 1 == i && ischar(arg)
                    iter.id = arg;
                    args = args(2:end);
                else 
                  if ischar(arg)
                    error('Simatra:Iterator', ['Unknown argument ' ...
                    '%s passed to Iterator'], arg);
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
                options = ['sample_period=' mat2str(iter.params('sample_period'))];
            else
                param_options = mapToString(iter.params);
                options = ['solver=' iter.solver '{' param_options '}'];
            end
            str = ['iterator ' iter.id ' with {' iter.type ', ' options '}'];
        end
        
        function str = toInfo(iter)
            if strcmpi(iter.type, 'discrete')
                options = ['sample_period=' num2str(iter.params('sample_period'))];
                str = sprintf('Discrete iterator with {%s}', options);
            else
                param_options = mapToString(iter.params);
                options = ['solver=' iter.solver '{' param_options '}'];
                str = sprintf('Continuous iterator with {%s}', options);
            end
        end
        
        function disp(iter)
            if strcmpi(iter.type, 'discrete')
                disp(['Discrete iterator ' iter.id])
            else
                disp(['Continuous iterator ' iter.id ' with solver ''' iter.solver ''''])
            end
            options = keys(iter.params);
            for i=1:length(options)
                disp(['  ' optionToString(options{i}, iter.params(options{i}))]);
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
                                if length(args) > i
                                    [varargout{:}] = feval(args(i).subs, m, args(i+1).subs{:});
                                else
                                    [varargout{:}] = feval(args(i).subs, m);
                                end
                            else
                                if length(args) > i
                                    feval(args(i).subs, m, args(i+1).subs{:});
                                else
                                    feval(args(i).subs, m);
                                end
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
                            key = args(i).subs;
                            switch(key)
                                case {'dt', 'abstol', 'reltol', 'sample_period', 'sample_frequency'}
                                    if ~isnumeric(val)
                                        error(['Simatra:Iterator:' key], ['The value passed for ' key ' must be a numeric quantity']);
                                    elseif val <= 0
                                        error(['Simatra:Iterator:' key], ['The value passed for ' key ' must be greater than zero']);
                                    end
                                    
                                    if strcmp(key, 'sample_period')
                                        m.params('sample_frequency') = 1/val;
                                    end
                                    if strcmp(key, 'sample_frequency')
                                        m.params('sample_period') = 1/val;
                                    end
                                    
                                case {'cv_lmm'}
                                    if ~ischar(val)
                                        error('Simatra:Iterator:cv_lmm', 'The value passed in for cv_lmm must be a string (one of CV_ADAMS or CV_BDF)');
                                    end
                                    switch(val)
                                        case {'CV_ADAMS', 'CV_BDF'}
                                            m.params('cv_lmm') = val;
                                        otherwise
                                            error('Simatra:Iterator:cv_lmm', 'The value passed in for cv_lmm must be either CV_ADAMS or CV_BDF');
                                    end
                                    
                                case {'cv_iter'}
                                    if ~ischar(val)
                                        error('Simatra:Iterator:cv_iter', 'The value passed in for cv_iter must be a string (one of CV_NEWTON or CV_FUNCTIONAL)');
                                    end
                                    switch(val)
                                        case {'CV_NEWTON', 'CV_FUNCTIONAL'}
                                            m.params('cv_iter') = val;
                                        otherwise
                                            error('Simatra:Iterator:cv_iter', 'The value passed in for cv_iter must be either CV_NEWTON or CV_FUNCTIONAL');
                                    end
                                    
                                otherwise
                                    % ignore right now
                            end
                            m.params(key) = val;
                        elseif strcmp('solver', args(i).subs)
                            if ~ischar(val)
                                error('Simatra:Iterator:solver', 'The value passed for solver must be a string quantity');
                            end
                            switch val
                                case m.solvers
                                    % save previous params
                                    prev_params = m.params;
                                    
                                    % define the new solver
                                    m.solver = val;
                                    
                                    % set the default parameters
                                    m.params = containers.Map;
                                    setDefaultParams(m);
                                    
                                    % update the default parameters
                                    % with those already set
                                    prev_opts = keys(prev_params);
                                    for j=1:length(prev_opts)
                                        opt = prev_opts{j};
                                        if isKey(m.params, opt)
                                            m.params(opt) = prev_params(opt);
                                        end
                                    end
                                    
                                otherwise
                                    error('Simatra:Iterator:solver', 'Invalid solver specified.  Options are: %s', List.cell2str(iter.solvers));
                            end
                        elseif any(strcmp(properties(m), args(i).subs))
                            m.(args(i).subs) = val;
                        else
                            error('Simatra:Iterator:subsasgn', ['There is no such method or property called ''' args(i).subs ''' in Iterator']);
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

function str = mapToString(map)

options = keys(map);
key_strings = cell(1,length(options));
for i=1:length(options)
    key_strings{i} = optionToString(options{i}, map(options{i}));
end
str = List.stringConcatWith(', ', key_strings);

end

function str = optionToString(key, value)

if ischar(value)
    value_str = ['"' value '"'];
elseif isnumeric(value) && isscalar(value)
    value_str = mat2str(value);
else
    error('Simatra:Iterator:optionToString', ['Option ' key ' passed in to iterator is not a valid type']);
end

str = [key '=' value_str];

end