% Suite class
%   Executes suites of simEngine tests
%
% Copyright 2009, 2010 Simatra Modeling Technologies, L.L.C.
classdef Suite < handle
   
    % define properties of the Suite
    properties
        Name;
        ExitOnFailure = false;
        ExitOnError = false;
        DeleteSIMs = true;     
    end
    
    properties (SetAccess = private)
        Tests = {};
        Total = 0;
        Passed = 0;
        Failed = 0;
        Errored = 0;
        Skipped = 0;
        Time = 0;
        Enabled = true;
        Dir
    end
    
    % define the instance methods
    methods
        
        % define the constructor
        function s = Suite(name, varargin)
            s.Name = name;
            s.Dir = pwd;
            if nargin == 2
                if iscell(varargin{1})
                    s.Tests = varargin{1};
                    s.Total = length(s);
                else
                    error('Suite:ArgumentError', 'Second argument must be a cell array')
                end
            end
                   
        end
        
        % add tests
        function add(s, obj)
            if isa(obj, 'Test') || isa(obj, 'Suite')
                s.Tests{length(s.Tests)+1} = obj;
            else
                error('Suite:AddTestOrSuite', 'Must pass in a test or a suite');
            end
            s.Total = length(s);
        end
        
        % helper functions
        function total = length(s)
            total = 0;
            for i=1:length(s.Tests)
               if isa(s.Tests{i}, 'Test')
                   total = total + 1;
               elseif isa(s.Tests{i}, 'Suite')
                   total = total + length(s.Tests{i});
               end 
            end
        end
        
        % execute tests
        function Execute(s, varargin)
            if s.Enabled
                if nargin == 2 && ischar(varargin{1})
                    switch lower(varargin{1})
                        case '-all'
                            execute_helper(s, 0, true, true);
                        otherwise
                            error('Suite:Execute:ArgumentError', 'Only -all is a supported argument');
                    end

                end
                execute_helper(s, 0, false, true);
            else
                s.Skipped = s.Skipped + length(s);
            end
        end
        
        function execute_helper(s, level, runall, runfailures)
        % delete any sim files that may exist in the working
        % directory
        if s.DeleteSIMs
          cwd = pwd;
          try
            cd(s.Dir);
            delete('*.sim');
            cd(cwd);
          catch          
            cd(cwd);
          end
        end
        spaces = blanks(level*2);
            disp(sprintf('\n%sRunning Suite ''%s'' (Total of %d tests)', spaces, s.Name, s.Total));
            localtime = tic;
            cont = true;
            for i=1:length(s.Tests)
                if isa(s.Tests{i}, 'Test')
                    t = s.Tests{i};          
                    % Check the previous result
                    switch t.Result
                        case t.NOT_EXECUTED
                            run_test = true;
                        case t.SKIPPED
                            run_test = true;
                            s.Skipped = s.Skipped - 1;
                        case t.PASSED
                            run_test = runall;
                            if run_test 
                                s.Passed = s.Passed - 1;
                            end
                        case t.FAILED
                            run_test = runall || runfailures;
                            if run_test
                                s.Failed = s.Failed - 1;
                            end
                        case t.ERROR
                            run_test = runall || runfailures;
                            if run_test
                                s.Errored = s.Errored - 1;
                            end
                    end
                    
                    if cont && run_test && t.Enabled
                        %disp(['Executing test ' t.Name])
                        t.Execute();
                        switch t.Result
                            case t.PASSED
                                s.Passed = s.Passed + 1;
                            case t.FAILED
                                s.Failed = s.Failed + 1;
                                if s.ExitOnFailure
                                    cont = false;
                                end
                            case t.ERROR
                                s.Errored = s.Errored + 1;
                                if s.ExitOnFailure || s.ExitOnError
                                    cont = false;
                                end
                        end
                    else
                        t.Skip();
                        s.Skipped = s.Skipped + 1;
                    end
                elseif isa(s.Tests{i}, 'Suite')
                    ss = s.Tests{i};
                    % Remove the previously computed totals
                    s.Passed = s.Passed - ss.Passed;
                    s.Failed = s.Failed - ss.Failed;
                    s.Errored = s.Errored - ss.Errored;
                    s.Skipped = s.Skipped - ss.Skipped;
                    % Now re-execute as necessary
                    if cont
                        if ss.Enabled 
                            ss.execute_helper(level+1, runall, runfailures);
                            s.Passed = s.Passed + ss.Passed;
                            s.Failed = s.Failed + ss.Failed;
                            s.Errored = s.Errored + ss.Errored;
                            s.Skipped = s.Skipped + ss.Skipped;
                        else
                            s.Skipped = s.Skipped + length(ss);
                        end
                    else
                        s.Skipped = s.Skipped + length(ss);
                    end
                end
            end
            totalTime = toc(localtime);
            s.Time = totalTime;

            disp(sprintf('%sSuite ''%s'' finished in %g seconds (Total=%d, Passed=%d, Failed=%d, Errored=%d, Skipped=%d)', spaces, s.Name, s.Time, s.Total, s.Passed, s.Failed, s.Errored, s.Skipped));
            
        end
        
        % override the disp function
        function disp(s)
            s.Total = s.length;
            disp(sprintf('Suite: %s', s.Name));
            disp('------------------------------------------');
            if s.Total == 0
                disp('  No Tests Defined');
            elseif s.Total == s.Passed
                disp(sprintf('  All Passed (%g out of %g, total time=%g)', s.Passed, s.Total, s.Time));
            elseif s.Passed == 0 && s.Failed == 0 && s.Errored == 0 && s.Skipped == 0
                disp(sprintf('  No Tests Executed (%g total tests)', s.Total));
            else
                if s.Passed > 0
                    disp(sprintf('  Passed: %d', s.Passed));
                end
                if s.Failed > 0
                    disp(sprintf('  Failed: %d', s.Failed));
                end
                if s.Errored > 0
                    disp(sprintf('  Errored: %d', s.Errored));
                end
                if s.Skipped > 0
                    disp(sprintf('  Skipped: %d', s.Skipped));
                end
                disp(sprintf('  Total Time: %g s', s.Time));
            end
        end
        
        function root = toXML (s, varargin)
            if 1 == nargin
                xml = com.mathworks.xml.XMLUtils.createDocument('testsuite');
                root = xml.getDocumentElement;
            else
                xml = varargin{1};
                parent = varargin{2};
                root = parent.appendChild(xml.createElement('testsuite'));
            end
            
            root.setAttribute('errors', num2str(s.Errored));
            root.setAttribute('tests', num2str(s.Total));
            root.setAttribute('time', num2str(s.Time));
            root.setAttribute('failures', num2str(s.Failed));
            root.setAttribute('name', s.Name);
            
            for i = 1:length(s.Tests)
                s.Tests{i}.toXML(xml, root);
            end
        end
        
        function writeXML(s, file)
            xmlwrite(file, s.toXML);
        end
      
        
        function Summary(s, varargin)
            disp(' ')
            disp('Suite Summary')
            disp('--------------------------')
            if nargin == 2
                if ischar(varargin{1}) 
                    if strcmpi(varargin{1},'-detailed')
                        summary_helper(s, 0, true, true, true);
                    elseif strcmpi(varargin{1},'-short')
                        summary_helper(s, 0, true, false, false);
                    elseif strcmpi(varargin{1},'-failures')
                        summary_helper(s, 0, false, false, true);
                    else
                        error('Suite:Summary:ArgumentError', 'Only -detailed, -short, or -failures flag is allowed');
                    end
                else
                    error('Suite:Summary:ArgumentError', 'Only -detailed, -short, or -failures flag is allowed');
                end
            else
                summary_helper(s, 0, true, false, true);
            end
            disp('--------------------------')
            disp(' ')
        end
        
        function summary_helper(s, level, showSuites, showTests, showFailures)
            spaces = blanks(level*2);
            base_str = sprintf('%sSuite ''%s'': ', spaces, s.Name);
            s.Total = s.length;
            if ~showSuites && ~showTests && showFailures && s.Failed == 0 && s.Errored ...
                  == 0
                summary_str = '';
            elseif s.Total == 0
                summary_str = 'No Tests Defined';
            elseif s.Total == s.Passed
                summary_str = sprintf('All PASSED (%d total tests, time=%g s)', s.Total, s.Time);                
            elseif s.Passed == 0 && s.Failed == 0 && s.Errored == 0 && s.Skipped == 0
                summary_str = sprintf('Suite not yet executed (%d total tests)', s.Total);
            else
                summary_str = sprintf('%d of %d Passed with %d Errored and %d Skipped (time=%g)', s.Passed, s.Total, s.Errored, s.Skipped, s.Time);
            end
            if ~isempty(summary_str)
              disp([base_str summary_str]);
            end
            for i=1:length(s.Tests)
                if isa(s.Tests{i},'Test') 
                    t = s.Tests{i};
                    if showTests
                        display([spaces '  ' s.Tests{i}.tostr]);
                    elseif showFailures && (t.Result == t.FAILED || t.Result == t.ERROR)
                        display([spaces '  ' s.Tests{i}.tostr]);                        
                    end
                elseif isa(s.Tests{i}, 'Suite')
                    summary_helper(s.Tests{i}, level+1, showSuites, ...
                                   showTests, showFailures);
                end
            end
        end
        
        % more accessors
        function list = getTests(s)
            list = cell(length(s.Tests),1);
            for i=1:length(s.Tests)
                list{i} = s.Tests{i}.Name;
            end
        end
        
        function t = getTest(s, name)
            for i=1:length(s.Tests)
                if strcmpi(s.Tests{i}.Name, name)
                    t = s.Tests{i};
                    return;
                end
            end
            disp(['All tests in suite ''' s.Name ''':'])
            disp(getTests(s));
            error('Simatra:Suite:TestNotFound', 'Can''t find test with name ''%s''',name);
        end
        
        function enable(s)
            s.Enabled = true;
        end
        function disable(s)
            s.Enabled = false;
        end
        
    end % end methods
    
end % end classdef Suite
