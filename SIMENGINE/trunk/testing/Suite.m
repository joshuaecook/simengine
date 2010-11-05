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
        Result
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

    properties (Access = protected)
        TagContainer
        TagCondition
    end
    
    % define the instance methods
    methods
        
        % define the constructor
        function s = Suite(name, varargin)
            s.Name = name;
            s.Dir = pwd;
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
            if isa(obj, 'Test') || isa(obj, 'Suite')
                s.Tests{length(s.Tests)+1} = obj;
                suite_tags = tags(s);
                obj.addTags(suite_tags{:});
                if nargin == 3 && iscell(extra_tags)
                    obj.addTags(extra_tags{:});
                end
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
            for i=1:length(s.Tests)
                ss = s.Tests{i};
                ss.addTags(varargin{:});
            end
        end
        
        function t = tags(s)
            t = keys(s.TagContainer);
        end
        
        % internal functoin = returns a containers.Map structure of the
        % tags and their counts
        function return_tags = countTags(s, tags)
            for i=1:length(s.Tests)
                t = s.Tests{i};
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
        function count = Count(s, condition)
            if nargin == 1
                condition = s.TagCondition;
            end
            count = 0;
            if isa(condition, 'Tag')
                for i = 1:length(s.Tests)
                    t = s.Tests{i};
                    if isa(t, 'Suite')
                        count = count + t.Count(condition);
                    else
                        count = count + test(condition, t.tags);
                    end
                end
            elseif ischar(condition)
                count = Count(s, Tag(condition));
            else
                error('Simatra:Suite:Count', 'Must pass in either a tag condition or a string tag name');
            end
        end
        
        % execute tests
        function Execute(s, varargin)
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
                        case '-tag'
                            if i == length(varargin)
                                error('Suite:Execute:ArgumentError', '-tag option requires an additional tag argument')
                            end
                            tag_arg = varargin{i+1};
                            switch class(tag_arg)
                                case 'Tag'
                                    condition = tag_arg;
                                case 'char'
                                    conv_str = regexprep(tag_arg, '(\w+)', 'Tag(''$1'')');
                                    try
                                        condition = eval(conv_str);
                                    catch me
                                        disp(getReport(me, 'extended'));
                                        error('Simatra:Suite:Execute', 'Can not process condition <%s>', conv_str);
                                    end
                                    if ~isa(condtion, 'Tag')
                                        error('Simatra:Suite:Execute', 'A tag was not generated out of the passed in string <%s>', tag_arg);
                                    end
                                otherwise
                                    error('Suite:Execute:ArgumentError', 'Unexpected non Tag or string -tag argument');
                            end
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
                elseif isa(s.Tests{i}, 'Suite')
                    ss = s.Tests{i};
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

            disp(sprintf('%sSuite ''%s'' finished in %g seconds (Total=%d, Passed=%d, Failed=%d, Errored=%d, Skipped=%d)', spaces, s.Name, s.Time, s.Total, s.Passed, s.Failed, s.Errored, s.Skipped));
            
        end
        
        function Skip(s)
            s.Result = Test.SKIPPED;
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
            % Make sure to ignore skipped tests in Bamboo output
            root.setAttribute('tests', num2str(s.Total - s.Skipped));
            root.setAttribute('time', num2str(s.Time));
            root.setAttribute('failures', num2str(s.Failed));
            root.setAttribute('name', s.Name);
            
            for i = 1:length(s.Tests)
              % Ignore skipped tests
              
              if s.Tests{i}.Enabled && not(s.Tests{i}.Result == Test.SKIPPED || s.Tests{i}.Result == Test.NOT_EXECUTED)
                s.Tests{i}.toXML(xml, root);
              end
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
                    elseif strcmpi(varargin{1},'-tags')
                        showTags(s);
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
        
        function showTags(s)
            counts = countTags(s, containers.Map);
            tags = keys(counts);
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
        
    end % end methods
    
end % end classdef Suite
