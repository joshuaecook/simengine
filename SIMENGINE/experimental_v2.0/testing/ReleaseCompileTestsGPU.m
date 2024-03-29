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
    if isempty(name)
        warning('Simatra:ReleaseCompileTestsGPU', 'Unexpected file %s', dsl_files{i});
    elseif name(1) ~= '.'
        filename = fullfile(path, [name ext]);
        s.add(Test(['Model-' name], @()(simex(filename,'-gpu')), '-withouterror'));
    end
end

  % The below test won't pass because the model name is different than the
  % file name.  This is expected
  t = s.getTest('Model-neuronWithSynapse'); t.ExpectFail = true;
  t = s.getTest('Model-circuit_elements'); t.ExpectFail = true;

  % These tests use cvode and we should add a compiler error message to check against, but for now, just expect them to fail on the GPU
  t = s.getTest('Model-lorenz'); t.ExpectFail = true;
  t = s.getTest('Model-purine'); t.ExpectFail = true;
  
  % Remove tests that are internal
  % These tests fail because they don't compile within the time
  % limit of the testing framework
  t = s.getTest('Model-axon'); t.addTags('backlog');
  t = s.getTest('Model-innersystem'); t.addTags('backlog');
  t = s.getTest('Model-solarsystem'); t.addTags('backlog');
  t = s.getTest('Model-song'); t.addTags('backlog');

end
