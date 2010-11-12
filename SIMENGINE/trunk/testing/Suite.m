% Suite class
%   Executes suites of simEngine tests
%
% Suite Methods:
%   Constructor:
%   Suite/Suite - constructor for a Suite object
%
%   Suite Actions:
%   add - add a Suite or a Test to a Suite object
%   Execute - run a Suite of tests
%   Tests - index into the suite
%   
%   Suite Logging:
%   Summary - list the tests/suites in the Suite object
%   showTags - list the tags in the Suite sorted by count
%   Count - return the number of tests matching a Tag condition
%   writeXML - write the test results to an XML file
%
% Copyright 2009, 2010 Simatra Modeling Technologies, L.L.C.
%
classdef Suite < handle
   
    % define properties of the Suite
    properties
        Name;
        ExitOnFailure = false;
        ExitOnError = false;
        DeleteSIMs = true;     
        Result
    end
    
    properties (SetAccess = private)
        %Tests = {};
        TestsMap;
        %Total = 0;
        Passed = 0;
        Failed = 0;
        Errored = 0;
        Skipped = 0;
        Time = 0;
        Enabled = true;
        Dir
        CountInSuite = 1;
    end

    properties (Access = protected)
        TagContainer
        TagCondition
    end
    
    % define the instance methods
    methods
        
        % define the constructor
        function s = Suite(name, varargin)
            % Suite - create a new suite object
            %
            % Usage:
            %   s = Suite(NAME [, {TAG1, TAG2, ...}]) - create a suite and
            %   optionally assign tags to the suite
            %
            % Examples:
            %   s = Suite('Performance', {'performance', 'fast'});
            %
            s.Name = name;
            s.Dir = pwd;
            s.TestsMap = containers.Map;
            s.TagContainer = containers.Map;
            s.TagCondition = Tag(false);
            s.Result = Test.NOT_EXECUTED;
            if nargin == 2
                if iscell(varargin{1})
                    suite_tags = varargin{1};
                    s.addTags(suite_tags{:});
                else
                    s.addTags(varargin{:})
                end
            end
            
        end
        
        % add tests
        function add(s, obj, extra_tags)
            % add - add a test or suite to a Suite object
            %
            % Usage:
            %   s.add(TEST|SUITE [, {TAG1, TAG2, ...}]) - add a test or
            %   suite with optional tags applied to that suite/test
            %
            % Examples:
            %   s.add(fast_cpu_test, {'fast', 'cpu'});
            %
            
            if isa(obj, 'Test') || isa(obj, 'Suite')
                key_str = sprintf('%04d: %s', s.CountInSuite, obj.Name);
                s.TestsMap(key_str) = obj;
                %s.Tests{length(s.Tests)+1} = obj;
                suite_tags = tags(s);
                obj.addTags(suite_tags{:});
                if nargin == 3 && iscell(extra_tags)
                    obj.addTags(extra_tags{:});
                end
            else
                error('Suite:AddTestOrSuite', 'Must pass in a test or a suite');
            end
            %s.Total = length(s);
            s.CountInSuite = s.CountInSuite + 1;
        end
        
        % helper functions
        function total = length(s)
            total = 0;
            tests = s.TestsMap;
            test_values = values(tests);
            for i=1:length(test_values)
                t = test_values{i};
                if isa(t, 'Suite')
                    total = total + length(t);
                else
                    total = total + 1;
                end
            end
        end
        
        % just a helper function
        function t = Total(s)
            t = length(s);
        end
                
        % subsref - pull out specific tests
        function t = Tests(s, index)
            if nargin == 1
                error('Simatra:Suite:Tests', 'Tests now takes one argument for the index <s.Tests(4) %% the fourth test>');
            end
            test_keys = keys(s.TestsMap);
            
            if ~isnumeric(index) || ~isscalar(index) || index < 1 || index > length(test_keys)
                error('Simatra:Suite:Tests', 'Only a scalar index is supported in Tests');
            end
            
            t = s.TestsMap(test_keys{index});
        end
        
        % addTags - add tags to the current suite
        function addTags(s, varargin)
            for i=1:length(varargin)
                tag = varargin{i};
                if ischar(tag)
                    s.TagContainer(tag) = true;
                else
                    error('Simatra:Test:addTags', 'All tags must be strings');
                end
            end
            % recurse through and add tags ...
            tests = values(s.TestsMap);
            for i=1:length(tests)
                ss = tests{i};
                ss.addTags(varargin{:});
            end
        end
        
        function t = tags(s)
            t = keys(s.TagContainer);
        end
        
        function setCondition(s, condition)
            s.TagCondition = toTag(condition);
        end
        
        % internal functoin = returns a containers.Map structure of the
        % tags and their counts
        function return_tags = countTags(s, tags)
            tests = values(s.TestsMap);
            for i=1:length(tests)
                t = tests{i};
                if isa(t, 'Test') 
                    test_tags = t.tags;
                    for i=1:length(test_tags)
                        tag = test_tags{i};
                        if isKey(tags, tag)
                            tags(tag) = tags(tag) + 1;
                        else
                            tags(tag) = 1;
                        end
                    end
                else
                    tags = t.countTags(tags);
                end
            end
            return_tags = tags;
        end
        
        % return the count of tests with a particular condition met
        function count = Count(s, tag)
            % COUNT - return the count of tests with satisfying a Tag
            % condition
            %
            % Usage:
            %   NUMTESTS = s.Count(CONDITION)
            %
            % Examples:
            %   numTests = s.Count('core&cpu&~internal');
            %
            if nargin == 1
                condition = s.TagCondition;
            else
                condition = toTag(tag);
            end
            count = 0;
            tests = values(s.TestsMap);
            for i = 1:length(tests)
                t = tests{i};
                if isa(t, 'Suite')
                    count = count + t.Count(condition);
                else
                    count = count + test(condition, t.tags);
                end
            end
        end
        
        % execute tests
        function Execute(s, varargin)
            % Execute - execute the tests contained in the suite object
            %
            % Usage
            %   s.Execute() - execute based on the default condition (no
            %   tests)
            %   s.Execute('-all') - execute all tests, even if they have
            %   already passed
            %   s.Execute('-tag', CONDITION) - execute all tests satisfying
            %   a Tag CONDITION
            %
            % Examples
            %   s.Execute('performance&gpu')
            %
            
            % set the some options to start
            runfailures = true;
            runall = false;
            condition = s.TagCondition;
            
            for i=1:length(varargin);
                arg = varargin{i};
                if ischar(arg)
                    switch lower(varargin{1})
                        case '-all'
                            runall = true;
                            condition = Tag(true);
                        case '-tag'
                            if i == length(varargin)
                                error('Suite:Execute:ArgumentError', '-tag option requires an additional tag argument')
                            end
                            tag_arg = varargin{i+1};
                            condition = toTag(tag_arg);
                            break;
                        otherwise
                            error('Suite:Execute:ArgumentError', 'Only -all and -tag are supported string arguments');
                    end
                elseif isa(arg, 'Tag')
                    condition = arg;
                else
                    error('Suite:Execute:ArgumentError', 'Only -all, a Tag object, or a -tag <char> are a supported arguments');
                end                    

            end
    
            disp(sprintf('Running %d tests in total using condition %s', s.Count(condition), toStr(condition)));
            
            if s.Enabled && s.Count(condition) > 0
                execute_helper(s, 0, runall, runfailures, condition);
            else
                s.Skipped = s.Skipped + length(s);
            end
        end
        
        function execute_helper(s, level, runall, runfailures, condition)
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
        total = Count(s, condition);
            disp(sprintf('\n%sRunning Suite ''%s'' (Total of %d tests)', spaces, s.Name, total));
            localtime = tic;
            cont = true;
            tests = values(s.TestsMap);
            for i=1:length(tests)
                if isa(tests{i}, 'Test')
                    t = tests{i};          
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
                    
                    if cont && run_test && t.Enabled && test(condition, t.tags)
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
                elseif isa(tests{i}, 'Suite')
                    ss = tests{i};
                    % Remove the previously computed totals
                    s.Passed = s.Passed - ss.Passed;
                    s.Failed = s.Failed - ss.Failed;
                    s.Errored = s.Errored - ss.Errored;
                    s.Skipped = s.Skipped - ss.Skipped;
                    % Now re-execute as necessary
                    if cont
                        if ss.Enabled && ss.Count(condition) > 0
                            ss.execute_helper(level+1, runall, runfailures, condition);
                            s.Passed = s.Passed + ss.Passed;
                            s.Failed = s.Failed + ss.Failed;
                            s.Errored = s.Errored + ss.Errored;
                            s.Skipped = s.Skipped + ss.Skipped;
                            if ss.Errored > 0
                                ss.Result = Test.ERROR;
                            elseif ss.Failed > 0
                                ss.Result = Test.FAILED;
                            else
                                ss.Result = Test.PASSED;
                            end
                        else
                            ss.Skip();
                            s.Skipped = s.Skipped + length(ss);
                        end
                    else
                        ss.Skip();
                        s.Skipped = s.Skipped + length(ss);
                    end
                end
            end
            totalTime = toc(localtime);
            s.Time = totalTime;

            disp(sprintf('%sSuite ''%s'' finished in %g seconds (Total=%d, Passed=%d, Failed=%d, Errored=%d, Skipped=%d)', spaces, s.Name, s.Time, length(s), s.Passed, s.Failed, s.Errored, s.Skipped));
            
        end
        
        function Skip(s)
            s.Result = Test.SKIPPED;
        end
        
        % override the disp function
        function disp(s)
            total = s.length;
            disp(sprintf('Suite: %s', s.Name));
            disp('------------------------------------------');
            if total == 0
                disp('  No Tests Defined');
            elseif total == s.Passed
                disp(sprintf('  All Passed (%g out of %g, total time=%g)', s.Passed, total, s.Time));
            elseif s.Passed == 0 && s.Failed == 0 && s.Errored == 0 && s.Skipped == 0
                disp(sprintf('  No Tests Executed (%g total tests)', total));
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
            % Make sure to ignore skipped tests in Bamboo output
            root.setAttribute('tests', num2str(length(s) - s.Skipped));
            root.setAttribute('time', num2str(s.Time));
            root.setAttribute('failures', num2str(s.Failed));
            root.setAttribute('name', s.Name);
            
            tests = values(s.TestsMap);
            for i = 1:length(tests)
              % Ignore skipped tests
              
              if tests{i}.Enabled && not(tests{i}.Result == Test.SKIPPED || tests{i}.Result == Test.NOT_EXECUTED)
                tests{i}.toXML(xml, root);
              end
            end
        end
        
        function writeXML(s, file)
            % writeXML - create an XML output
            %
            % Usage:
            %   s.writeXML(FILENAME) - output XML to the specified FILENAME
            xmlwrite(file, s.toXML);
        end
      
        
        function Summary(s, varargin)
            disp(' ')
            disp('Suite Summary')
            disp('--------------------------')
            if nargin >= 2
                if ischar(varargin{1}) 
                    if strcmpi(varargin{1},'-detailed')
                        summary_helper(s, 0, true, true, true);
                    elseif strcmpi(varargin{1},'-short')
                        summary_helper(s, 0, true, false, false);
                    elseif strcmpi(varargin{1},'-failures')
                        summary_helper(s, 0, false, false, true);
                    elseif strcmpi(varargin{1},'-tags')
                        showTags(s);
                    elseif strcmpi(varargin{1},'-tag') && length(varargin) > 1
                        summary_helper(s, 0, true, true, true, toTag(varargin{2}));
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
        
        function summary_helper(s, level, showSuites, showTests, showFailures, condition)
            if nargin == 5
                condition = Tag(true);
            end
            spaces = blanks(level*2);
            base_str = sprintf('%sSuite ''%s'': ', spaces, s.Name);
            total = s.length;
            if ~showSuites && ~showTests && showFailures && s.Failed == 0 && s.Errored ...
                  == 0
                summary_str = '';
            elseif total == 0
                summary_str = 'No Tests Defined';
            elseif total == s.Passed
                summary_str = sprintf('All PASSED (%d total tests, time=%g s)', total, s.Time);                
            elseif s.Passed == 0 && s.Failed == 0 && s.Errored == 0 && s.Skipped == 0
                count_matching_condition = s.Count(condition);
                if count_matching_condition == total
                    summary_str = sprintf('Suite not yet executed (%d total tests)', total);
                else
                    summary_str = sprintf('Suite not yet executed (%d of %d total tests)', count_matching_condition, total);
                end                    
            else
                summary_str = sprintf('%d of %d Passed with %d Errored and %d Skipped (time=%g)', s.Passed, total, s.Errored, s.Skipped, s.Time);
            end
            if ~isempty(summary_str)
              disp([base_str summary_str]);
            end
            tests = values(s.TestsMap);
            for i=1:length(tests)
                t = tests{i};
                if isa(t,'Test') && test(condition, t.tags)
                    if showTests
                        display([spaces '  ' t.tostr '  ' List.cell2str(t.tags)]);
                    elseif showFailures && (t.Result == t.FAILED || t.Result == t.ERROR)
                        display([spaces '  ' t.tostr]);
                    end
                elseif isa(t, 'Suite') && t.Count(condition) > 0
                    summary_helper(t, level+1, showSuites, ...
                        showTests, showFailures, condition);
                end
            end
        end
        
        % more accessors
        function list = getTests(s)
            %list = cell(length(s.TestsMap),1);
            list = keys(s.TestsMap);
            %for i=1:length(s.Tests)
            %    list{i} = s.Tests{i}.Name;
            %end
        end
        
        function t = getTest(s, name)
            all_tests = values(s.TestsMap);
            tests = List.map (@(t)(t.Name), all_tests);
            matching = find(strcmpi(tests, name));
            if isempty(matching)
                disp(['All tests in suite ''' s.Name ''':'])
                disp(getTests(s));
                error('Simatra:Suite:TestNotFound', 'Can''t find test with name ''%s''',name);
            elseif length(matching) == 1
                t = all_tests{matching};
            else
                error('Simatra:Suite:getTest', 'More than one matching test found with name ''%s''', name);
            end
        end
        
        function enable(s)
            s.Enabled = true;
        end
        function disable(s)
            s.Enabled = false;
        end
        
        function showTags(s)
            counts = countTags(s, containers.Map);
            tags = keys(counts);
            if isempty(tags)
                disp('<no tags defined>');
            else
                str_length = 0;
                count_list = zeros(length(tags), 2);
                count_list(:,2) = 1:length(tags);
                for i=1:length(tags)
                    l = length(tags{i});
                    if l > str_length
                        str_length = l;
                    end
                    count_list(i,1) = counts(tags{i});
                end
                sorted_count_list = sortrows(count_list, -1);
                pad = @(str)([str ':' blanks(str_length-length(str)+1)]);
                for i = 1:length(tags)
                    disp(['  ' pad(tags{sorted_count_list(i,2)}) num2str(sorted_count_list(i,1))]);
                end
            end
        end
        

        
    end % end methods
    
end % end classdef Suite

function tag = toTag(val)
switch class(val)
    case 'Tag'
        tag = val;
    case 'char'
        conv_str = regexprep(val, '([^\&\(\)\|\~]+)', 'Tag(''$1'')');
        try
            tag = eval(conv_str);
        catch me
            disp(getReport(me, 'extended'));
            error('Simatra:Suite:charToTag', 'Can not process condition <%s>', conv_str);
        end
        if ~isa(tag, 'Tag')
            error('Simatra:Suite:charToTag', 'A tag was not generated out of the passed in string <%s>', val);
        end
    case 'logical'
        tag = Tag(val);
    otherwise
        error('Simatra:Suite:charToTag:ArgumentError', 'Unexpected non Tag or string argument');
end
end