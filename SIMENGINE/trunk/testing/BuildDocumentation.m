% BuildDocumentation - create a test framework that builds the
% documentation
function s = BuildDocumentation

s = Suite('Documentation', {'documentation'});
s.setCondition(true);

% first check to see if the directory system is valid
makefile = fullfile('..', 'external-publications', 'Makefile');
if ~exist(makefile, 'file')
    return;
end

% return the list of documetation files to generate
make_target = 'html-targets';
result = run_make(make_target);
lines = regexp(result, '\n', 'split');
lines = List.filter (@(line)(~isempty(line)), lines);

% pull out the file names from each line
doc_targets = containers.Map;
for i=1:length(lines)
    t = regexp(lines{i}, '^(\S+):\s+(.*)$', 'tokens');
    key = t{1}{1};
    docs = regexp(t{1}{2}, '\s+', 'split');
    docs = List.filter (@(str)(~isempty(str)), docs);
    doc_targets(key) = docs;
end

% create a suite around each key and a test around each doc
suite_keys = keys(doc_targets);
for i=1:length(suite_keys)
    sub_suite = Suite(['Documentation set ' suite_keys{i}]);
    files = doc_targets(suite_keys{i});
    for j=1:length(files)
        file = files{j};
        test_fcn = @()(run_make([file ' DOCUMENTATION_OUTPUT_DIR=html']));
        t = Test(['Building ' file], test_fcn, '-withoutError');
        sub_suite.add(t);
    end
    if ~isempty(files)
        s.add(sub_suite);
    end 
end

% if there's no return argument, just run the suite
if nargout == 0
    s.Execute;
end

end

function [result] = run_make(target, directory)

if nargin == 1
    directory = '../external-publications';
end

make_cmd = ['make -C' directory ' -s SIMENGINE=../local-install doc-base ' target];
[status, result] = system(make_cmd);
    
if status ~= 0
    error('Simatra:run_make', 'Unexpected non-zero status, returned:\n%s', result);
end


end