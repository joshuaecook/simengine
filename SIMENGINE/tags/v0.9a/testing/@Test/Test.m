% TEST class
%   Performs individual tests of simEngine
%
% Construction
%   t = TEST(NAME, FUNCTION)
%     This will create a new test with name NAME running FUNCTION.
%     FUNCTION should have no input arguments and return a boolean true or
%     false.
%
%   t = TEST(NAME, FUNCTION, '-withouterror')
%     This test succeeds if the function executes without any errors.  This
%     mode is useful for compilation tests.
%
%   t = TEST(NAME, FUNCTION, '-equal', VAL)
%     This test succeeds if the return of FUNCTION is equivalent to VAL.
%     Equivalency is determined by the EQUIV helper function.
%
%   t = TEST(NAME, FUNCTION, '-approxequal', VAL [, PRECISION])
%     This test verifies that the return of FUNCTION is equal to VAL within
%     +/- PRECISION percent.
%
%   t = TEST(NAME, FUNCTION, '-range', LOW, HIGH)
%     This test verifies that the return of FUNCTION is greater or equal to
%     LOW and less or equal to HIGH.
%
% Usage
%
%  t.Execute - this will execute the test manually.  In generally, suites
%  will invoke this method of a test.
%
% Example
%  s = Suite('My Test Suite')
%  s.add(Test('Compile Sine Model', @()(simex('sine.dsl')),
%  '-withouterror'))
%  s.Execute
%
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
%
classdef Test < handle

    properties
        Name
        Function
        Dir
        Mode
    end
    
    % enumeration for the Result
    properties (Constant = true)
        NOT_EXECUTED = 0;
        PASSED = 1;
        FAILED = 2;
        ERROR = 3;
        SKIPPED = 4;
    end
    
    % enumeration for the Mode
    properties (Constant = true)
        BOOLEAN = 0;
        NOTERROR = 1;
        EQUAL = 2;
        APPROXEQUAL = 3;
        RANGE = 4;
    end
    
    properties (SetAccess = private)
        Executed
        Result
        Return
        Time
        CompareOptions
        Message = ''
    end
    
   
    methods
        
        
        function t = Test(name, func, varargin)
            if nargin >= 2
                t.Name = name;
                t.Function = func;
                t.Dir = pwd;
                t.Executed = false;
                t.Result = t.NOT_EXECUTED;
                t.Time = 0;
                t.Mode = t.BOOLEAN;
            else
                error('Test:ArgumentError', 'Wrong number of arguments');
            end
            if nargin >= 3 && isstr(varargin{1})
                switch lower(varargin{1})
                    case '-boolean'
                        t.Mode = t.BOOLEAN;
                    case '-withouterror'
                        t.Mode = t.NOTERROR;
                    case '-equal'
                        t.Mode = t.EQUAL;
                        if nargin == 4 && isnumeric(varargin{2})
                            t.CompareOptions = varargin{2};
                        else
                            error('Test:ArgumentError', 'When using the -equal option, there should be a fourth required option')
                        end
                    case '-approxequal'
                        t.Mode = t.APPROXEQUAL;
                        if nargin == 4 && isnumeric(varargin{2})
                            t.CompareOptions = [varargin{2} 1]; % by default, stay within one percent
                        elseif nargin == 5 && isnumeric(varargin{2}) && isnumeric(varargin{3})
                            t.CompareOptions = [varargin{2} varargin{3}]; % by default, stay within one percent
                        else
                            error('Test:ArgumentError', 'When using the -approxequal option, there should be a fourth required option and a fifth optional argument')
                        end
                    case '-range'
                        t.Mode = t.RANGE;
                        if nargin == 4 && length(varargin{2}) == 2 && isnumeric(varargin{2})
                            t.CompareOptions = varargin{2};
                        else
                            error('Test:ArgumentError', 'When using the -equal option, there should be a fourth required option')
                        end                        
                    otherwise
                        error('Test:ArgumentError','Unexpected third argument to Test')
                end
            elseif nargin >= 3 && not(isstr(varargin{1}))
                error('Test:ArgumentError', 'Expected string as third argument')
            end
        end
        
        function Execute(t)
            curDir = pwd;
            try
                cd(t.Dir);
                localtime = tic;
                if isstr(t.Function)
                    t.Return = feval(t.Function);
                else
                    finfo = functions(t.Function);
                    t.Return = t.Function();
                end
                t.Time = toc(localtime);
                
                % check the output
                switch t.Mode
                    case t.NOTERROR
                        t.Result = t.PASSED;
                    case t.BOOLEAN
                        if t.Return
                            t.Result = t.PASSED;
                        else
                            t.Result = t.FAILED;
                        end
                    case t.EQUAL
                        if equiv(t.Return, t.CompareOptions)
                            t.Result = t.PASSED;
                        else
                            t.Result = t.FAILED;
                            t.Message = ['Returned ''' num2str(t.Return) ''' instead'];
                        end
                    case t.APPROXEQUAL
                        if isnumeric(t.Return) && length(t.Return) == 1
                            l = t.CompareOptions(1)*(100-t.CompareOptions(2))/100;
                            h = t.CompareOptions(1)*(100+t.CompareOptions(2))/100;  
                            if t.Return <= h && t.Return >= l
                                t.Result = t.PASSED;
                            else
                                t.Result = t.FAILED;
                                t.Message = sprintf('Returned ''%s'' instead, which is not +/- %g%% of %g', num2str(t.Return), t.CompareOptions(2), t.CompareOptions(1));
                            end
                        else
                            t.Message = ['Unexpected return value ''' num2str(t.Return) ''''];
                            error('Test:ExecuteError:ApproxEqual', 'Return value ''%s'' not numeric or does not have length of one', num2str(t.Return));
                        end
                    case t.RANGE
                       if isnumeric(t.Return) && length(t.Return) == 1
                            l = t.CompareOptions(1);
                            h = t.CompareOptions(2);  
                            if t.Return <= h && t.Return >= l
                                t.Result = t.PASSED;
                            else
                                t.Result = t.FAILED;
                                t.Message = sprintf('Returned ''%s'' instead, which is not within [%g,%g]', num2str(t.Return), t.CompareOptions(1), t.CompareOptions(2));
                            end
                        else
                            t.Message = ['Unexpected return value ''' num2str(t.Return) ''''];
                            error('Test:ExecuteError:Range', 'Return value ''%s'' not numeric or does not have length of one', num2str(t.Return));
                       end
                    otherwise
                        error('Test:ExecuteError', 'Unknown mode encountered');
                end
            catch me
                t.Return = me;
                if t.Mode == t.NOTERROR
                    t.Result = t.FAILED;
                    t.Message = me.message;
                else
                    t.Result = t.ERROR;
                    t.Message = me.message;
                end
            end
            cd(curDir);
        end
        
        function Skip(t)
            t.Result = t.SKIPPED;
        end
        
        
        function s = tostr(t)
            if isempty(t.Message)
                s = sprintf('Test ''%s'': %s', t.Name, result2str(t.Result));
            else
                s = sprintf('Test ''%s'': %s (%s)', t.Name, result2str(t.Result), t.Message);
            end        
            if t.Result ~= t.NOT_EXECUTED 
                s = sprintf('%s [time=%gs]', s, t.Time);
            end
        end
        
        % override the display function
        function disp(t)
            disp(tostr(t));
        end
        
    end % methods
end % end class Test


% Additional helper functions
function r = result2str(result)
switch result
    case 0
        r = 'Not Executed';
    case 1
        r = 'Passed';
    case 2
        r = 'Failed';
    case 3
        r = 'Error occurred';
    case 4
        r = 'Skipped';
    otherwise
        r = 'Unknown';
end
end




