% Run this script to execute the full matlab test framework
% Copyright 2009 Simatra Modeling Technologies, L.L.C.
function matlab_test_script(mode_str)

mode = -1;
INTERNAL = 0;
RELEASE = 1;

% test input
switch lower(mode_str)
 case {'-release'}
  mode = RELEASE;
 case {'-internal'}
  mode = INTERNAL;
 otherwise
  error('Simatra:matlab_test_script', 'Argument Error');
end

% determine architecture
switch computer
 case {'PCWIN', 'GLNX86', 'MACI', 'MACI64'}
  local_install = '/local-install';
 case {'PCWIN64', 'GLNXA64', 'SOL64'}
  local_install = '/local-install-64';
 otherwise
  warning('Simatra:matlab_test_script', ['Architecture is not officially '...
                      'supported'])
end  

% first, set the path
p = mfilename('fullpath');
[path, file, ext] = fileparts(p);
cd([path local_install])
addpath(pwd)

% next, go into testing directory
cd ../testing

% grab all the tests (right now do all tests, not just release
% tests)
if mode == RELEASE
  %s = AllCPUTests('-release');
s = CoreFeatureTests('-cpu', '-release');
else
  %s = AllCPUTests('-internal');
  s = CoreFeatureTests('-internal');
end

% Execute
s.Execute

% Print the summary at the bottom after some blank lines
for i=1:5
  disp(' ');
end
s.Summary

s.writeXML('bamboolog.xml');

exit
end
