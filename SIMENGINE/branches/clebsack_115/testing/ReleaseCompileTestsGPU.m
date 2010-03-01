function s = ReleaseCompileTestsGPU

% grab the buildEngine path
buildenginepath = which('simex');
[installpath,filename,ext] = fileparts(buildenginepath);

% determine the example path
examplepath = fullfile(installpath, 'examples');

dsl_files_str = ls('-1',fullfile(examplepath,'/*/*.dsl'));
dsl_files = strread(dsl_files_str, '%s', 'delimiter', sprintf('\n'));

% create a suite of tests
s = Suite('Release Compile Tests GPU');

% add each of the dsl files to a run script
for i=1:length(dsl_files)
    [path, name, ext] = fileparts(dsl_files{i});
    if name(1) ~= '.'
        filename = fullfile(path, [name ext]);
        s.add(Test(['Model-' name], @()(simex(filename,'-gpu')), '-withouterror'));
    end
end

% The below test won't pass because the model name is different than the
% file name.  This is expected
s.getTest('Model-neuronWithSynapse').ExpectFail = true;

% These tests use cvode and we should add a compiler error message to check against, but for now, just expect them to fail on the GPU
s.getTest('Model-timingNetwork').ExpectFail = true;
s.getTest('Model-lorenz').ExpectFail = true;
s.getTest('Model-purine').ExpectFail = true;

end
