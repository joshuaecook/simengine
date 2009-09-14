function s = InternalCompileTests


% determine the example path
examplepath = '../examples';

dsl_files_str = [ls('-1',fullfile(examplepath,'/*/*.dsl')) ls('-1',fullfile(examplepath,'/*/*/*.dsl'))];
dsl_files = strread(dsl_files_str, '%s', 'delimiter', sprintf('\n'));

% create a suite of tests
s = Suite('InternalCompileTests');

% add each of the dsl files to a run script
for i=1:length(dsl_files)
    [path, name, ext] = fileparts(dsl_files{i});
    if name(1) ~= '.'
        filename = fullfile(path, [name ext]);
        s.add(Test(['Model ' name], @()(simex(filename)), '-withouterror'));
    end
end

end