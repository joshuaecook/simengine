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
%   t = TEST(NAME, FUNCTION, '-allequal')
%     This test succeeds if each parallel entry in the return structure of
%     FUNCTION is equal to every other. (returns true when running a single
%     model.
%
%   t = TEST(NAME, FUNCTION, '-range', LOW, HIGH)
%     This test verifies that the return of FUNCTION is greater or equal to
%     LOW and less or equal to HIGH.
%
%   t = TEST(NAME, FUNCTION, '-regexpmatch', STRING)
%     This test verifies that the stdout matches some regular expression 
%     pattern.  This is often paired with the 'expectfail' option when we
%     are testing an error output.
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
        ExpectFail = false
        ExpectError = false % testing an error condition
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
        REGEXP = 5;
        ALLEQUAL = 6;
    end
    
    properties (SetAccess = private)
        Executed
        Result
        Return
        Output
        Time
        CompareOptions
        Message = ''
    end
    
    properties (Access = private)
        FunctionFile
        FunctionName
        FunctionLine
        OutputFileName
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
            
            % Find the filename and line #
            try
              me = MException('Simatra:Test:Exception',['No Error ' ...
                                  'Occurred']);
              throw(me);
            catch me
              if length(me.stack) >= 2
                FunctionFile = me.stack(2).file;
                t.FunctionName = me.stack(2).name;
                t.FunctionLine = me.stack(2).line;
                [fpath,fname,fext]=fileparts(FunctionFile);
                t.FunctionFile = [fname fext];
              else
                t.FunctionName = 'N/A';
                t.FunctionLine = 0;
                t.FunctionFile = 'Matlab Interpreter';
              end
            end              
            
            if nargin >= 3 && ischar(varargin{1})
                switch lower(varargin{1})
                    case '-boolean'
                        t.Mode = t.BOOLEAN;
                    case '-withouterror'
                        t.Mode = t.NOTERROR;
                    case '-equal'
                        t.Mode = t.EQUAL;
                        if nargin == 4
                            t.CompareOptions = varargin(2);
                        else
                            error('Test:ArgumentError', 'When using the -equal option, there should be a fourth required option')
                        end
		    case '-allequal'
                        t.Mode = t.ALLEQUAL;
                    case '-approxequal'
                        t.Mode = t.APPROXEQUAL;
                        if nargin == 4
                            t.CompareOptions = {varargin{2}, 1}; % by default, stay within one percent
                        elseif nargin == 5 && isnumeric(varargin{3})
                            t.CompareOptions = {varargin{2}, varargin{3}}; % by default, stay within one percent
                        else
                            error('Test:ArgumentError', 'When using the -approxequal option, there should be a fourth required option and a fifth optional argument')
                        end
                    case '-range'
                        t.Mode = t.RANGE;
                        if nargin == 4 && length(varargin{2}) == 2 && isnumeric(varargin{2})
                            t.CompareOptions = varargin(2);
                        else
                            error('Test:ArgumentError', 'When using the -range option, there should be a fourth required option of length 2')
                        end      
                    case '-regexpmatch'
                        t.Mode = t.REGEXP;
                        if nargin == 4 && ischar(varargin{2})
                            t.CompareOptions = varargin(2);
                        else
                            error('Test:ArgumentError', 'When using the -regexpmatch option, there should be a required string option');
                        end
                    otherwise
                        error('Test:ArgumentError','Unexpected third argument to Test')
                end
            elseif nargin >= 3 && not(ischar(varargin{1}))
                error('Test:ArgumentError', 'Expected string as third argument')
            end
        end
        
        function Execute(t)
        curDir = pwd;
        cd(t.Dir);
        %                localtime = tic;
        %                if ischar(t.Function)
        %                    [t.Output, t.Return] = evalc('feval(t.Function)');
        %                else
        %                    finfo = functions(t.Function);
        %                    [t.Output, t.Return] = evalc('feval(t.Function)');
        %                end
        %                t.Time = toc(localtime);
        [t.Output, t.Return, t.Time, errored] = executeFunction(t.Function);
        if ~errored 
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
            if equiv(t.Return, t.CompareOptions{1})
              t.Result = t.PASSED;
            else
              t.Result = t.FAILED;
              if isnumeric(t.Return)
                t.Message = ['Returned ''' num2str(t.Return) ''' instead'];
              elseif isstruct(t.Return)
                t.Message = ['Returned a different structure instead'];
              else
                t.Message = ['Returned a different quantity instead'];
              end
            end
           case t.ALLEQUAL
            if all_equiv(t.Return)
              t.Result = t.PASSED;
            else
              t.Result = t.FAILED;
              t.Message = ['Not all parallel structures are identical'];
            end
           case t.APPROXEQUAL
            if approx_equiv(t.CompareOptions{1}, t.Return, t.CompareOptions{2})
              t.Result = t.PASSED;
            else
              t.Result = t.FAILED;
              %t.Message = sprintf('Returned ''%s'' instead, which is not +/- %g%% of %g', num2str(t.Return), t.CompareOptions(2), t.CompareOptions(1));
              if isnumeric(t.Return)
                t.Message = ['Returned ''' num2str(t.Return) ''' instead'];
              elseif isstruct(t.Return)
                t.Message = ['Returned a different structure instead'];
              else
                t.Message = ['Returned a different quantity instead'];
                %error('Test:ExecuteError:ApproxEqual', 'Return value ''%s'' not numeric or does not have length of one', num2str(t.Return));
              end
            end
           case t.RANGE
            if isnumeric(t.Return) && length(t.Return) == 1
              l = t.CompareOptions{1};
              h = t.CompareOptions{2};  
              if t.Return <= h && t.Return >= l
                t.Result = t.PASSED;
              else
                t.Result = t.FAILED;
                t.Message = sprintf('Returned ''%s'' instead, which is not within [%g,%g]', num2str(t.Return), t.CompareOptions{1}, t.CompareOptions{2});
              end
            else
              t.Message = ['Unexpected return value ''' num2str(t.Return) ''''];
              error('Test:ExecuteError:Range', 'Return value ''%s'' not numeric or does not have length of one', num2str(t.Return));
            end
           case t.REGEXP
            matches = regexp(t.Output, ...
                             t.CompareOptions{1});
            if isempty(matches)
              t.Result = t.FAILED;
              t.Message = sprintf('Regular expression ''%s'' not found in command output', t.CompareOptions{1});
            else
              t.Result = t.PASSED;
            end
           otherwise
            error('Test:ExecuteError', 'Unknown mode encountered');
          end
        else % an error occured
          switch t.Mode
           case t.REGEXP % in REGEXP mode, we don't care if an
                         % error occured, since we are often
                         % looking for error messages
            matches = regexp(t.Output, ...
                             t.CompareOptions{1});
            if isempty(matches)
              t.Result = t.FAILED;
              t.Message = sprintf('Regular expression ''%s'' not found in command output', t.CompareOptions{1});
            else
              t.Result = t.PASSED;
            end
           otherwise
            if t.Mode == t.NOTERROR
              t.Result = t.FAILED;
              t.Message = t.Return.message;
            else
              t.Result = t.ERROR;
              t.Message = t.Return.message;
            end
          end
        end
        
        % Check if expected fail
        if t.ExpectFail
          if t.Result == t.PASSED 
            t.Result = t.FAILED;
            t.Message = [t.Message '(Test passed, but expected to FAIL)'];
          elseif t.Result == t.FAILED
            t.Result = t.PASSED;
            t.Message = [t.Message ' (Expected to FAIL)'];
          end
        end 
        
        % Location string
        location = ['(' t.FunctionFile ':' num2str(t.FunctionLine) ')'];
        
        % Show result of this test
        if t.Result == t.PASSED
          status = 'Passed';
        elseif t.Result == t.FAILED
          status = ['FAILED  <---- ' location];
        else
          status = ['ERRORED <---- ' location];
        end
        s = sprintf('%40s:\t%s', t.Name, status);
        disp(s)
        
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

% Run the funname and return all the data/output
function [output, data, time, errored] = executeFunction (funname)

[output, data, time, errored] = evalc('executeFunctionHelper(funname)');

end

function [data, time, errored] = executeFunctionHelper(funname)
t = tic;
try
  data = feval(funname);
  errored = false;
catch me
  data = me;
  errored = true;
end
time = toc(t);
end

% read the output data file
function str = readOutputFile(filename)

if exist(filename, 'file')
    strcell = textread(filename, '%s', 'whitespace', '');
    if ~isempty(strcell)
        str = strcell{1};
    else
        str = '';
    end
    delete(filename);
else
    str = '';
end

end

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




