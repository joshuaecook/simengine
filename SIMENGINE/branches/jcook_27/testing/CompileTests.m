function s = CompileTests

% grab the buildEngine path
buildenginepath = which('simex');
[installpath,filename,ext] = fileparts(buildenginepath);

% determine the example path
examplepath = fullfile(installpath, 'examples');

dsl_files = dir(fullfile(examplepath,'*.dsl'));

% create a suite of tests
s = Suite('CompileTests');

% add each of the dsl files to a run script
for i=1:length(dsl_files)
    [path, name, ext] = fileparts(dsl_files(i).name);
    if name(1) ~= '.'
        filename = fullfile(examplepath, [name ext]);
        s.add(Test(['Model ' name], @()(simex(filename)), '-withouterror'));
    end
end

end