function s = ReleaseCompileTestsGPU(varargin)

if nargin == 1 && strcmpi(varargin{1}, '-internal')
  INTERNAL = 1;
else
  INTERNAL = 0;
end

% grab the buildEngine path
buildenginepath = which('simex');
[installpath,filename,ext] = fileparts(buildenginepath);

% determine the example path
examplepath = fullfile(installpath, 'examples');

dsl_files_str = [ls('-1',fullfile(examplepath,'/*/*.dsl')) ls('-1',fullfile(examplepath,'/*/*/*.dsl'))];
dsl_files = strread(dsl_files_str, '%s', 'delimiter', sprintf('\n'));

% create a suite of tests
s = Suite('Release Compile Tests GPU');

% add each of the dsl files to a run script
for i=1:length(dsl_files)
    [path, name, ext] = fileparts(dsl_files{i});
    if not(strcmp(name(1), '.'))
        filename = fullfile(path, [name ext]);
        s.add(Test(['Model-' name], @()(simex(filename,'-gpu')), '-withouterror'));
    end
end

  % The below test won't pass because the model name is different than the
  % file name.  This is expected
  s.getTest('Model-neuronWithSynapse').ExpectFail = true;
  s.getTest('Model-circuit_elements').ExpectFail = true;

  % These tests use cvode and we should add a compiler error message to check against, but for now, just expect them to fail on the GPU
  s.getTest('Model-lorenz').ExpectFail = true;
  s.getTest('Model-purine').ExpectFail = true;
  
% Remove tests that are internal
if ~INTERNAL
  % These tests fail because they don't compile within the time
  % limit of the testing framework
  s.getTest('Model-axon').Enabled = false;
  s.getTest('Model-innersystem').Enabled = false;
  s.getTest('Model-solarsystem').Enabled = false;
  s.getTest('Model-song').Enabled = false;
end

end
