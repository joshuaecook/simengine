function s = ReleaseCompileTests(varargin)

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
s = Suite('Release Compile Tests');

% add each of the dsl files to a run script
for i=1:length(dsl_files)
    [path, name, ext] = fileparts(dsl_files{i});
    if name(1) ~= '.'
        filename = fullfile(path, [name ext]);
        s.add(Test(['Model-' name], @()(simex(filename)), '-withouterror'));
    end
end

% The below test won't pass because the model name is different than the
% file name.  This is expected
s.getTest('Model-neuronWithSynapse').ExpectFail = true;
s.getTest('Model-circuit_elements').ExpectFail = true;

% Remove tests that are internal
if ~INTERNAL
  % These tests fail because they don't compile within the time
  % limit of the testing framework
  s.getTest('Model-axon').Enabled = false;
end

end
