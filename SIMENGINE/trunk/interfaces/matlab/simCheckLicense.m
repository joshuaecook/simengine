% SIMLICENSECHECK
%  Performs license related checks in simEngine
%
% Copyright 2010 Simatra Modeling Technologies
%
function [varargout] = simCheckLicense

% Check output arguments
if 1 < nargout 
  error('Simatra:simCheckLicense', ['Only one output argument is ' ...
                      'supported']);
  end
  

quiet = false;

json = execSimEngine('LF licenseToJSON()', quiet);
license = parse_json(json);

% Read license file
str_array = parseLicense()
license = evaluateLicense(str_array, quiet);

% Return the license if there's an output argument
if 1 == nargout
  varargout{1} = license;
end


end

% process the license as a cell array
function license = evaluateLicense(str_array, quiet)

% exit if the string array is not valid
if 0 == length(str_array)
  license = false;
  return
end

lic_str = [];
for i=1:(length(str_array)-1)
  lic_str = [lic_str '\"' str_array{i} '\", '];
end
lic_str = [lic_str '\"' str_array{end}  '\"'];

% test this license
cmd = ['LF licenseToJSON(' lic_str ')'];
disp(cmd)
json = execSimEngine(['LF licenseToJSON(' lic_str ')'], quiet)
license = parse_json(json);

end


% Grab the license from the command line
function str_array = parseLicense()

ATEND = @(str)(not(isempty(regexp(str, '.*==$'))));

linecount = 1;
str = '';
 
str_array = {};
while not(ATEND(str))
  if 1 == linecount 
    str = input('Copy license data into this prompt: ', 's');
  else
    str = input('', 's');
  end
  str_array{linecount} = str;
  linecount = linecount + 1;
end

end


% Run simEngine command and return result
function result = execSimEngine(cmd, quiet)

[pathstr, name, ext] = fileparts(which('simex'));
simEngine = fullfile(pathstr, 'bin', 'simEngine');
print_cmd = ['println(' cmd ')'];
echo_cmd = ['echo "' print_cmd '"'];
arguments = [' -batch - -startupmessage=false'];

full_cmd = [echo_cmd ' | ' simEngine arguments];

[status, result] = system(full_cmd);

% chomp the trailing new line
if 10 == double(result(end))
    result = result(1:(end-1));
end

% % DEBUG INFO
%if not(quiet)
%    disp(['Default download location: ' result]);
%end
%

if status ~= 0
    if not(quiet)
        disp(['Command: ' full_cmd]);
        disp(['Status: ' num2str(status)]);
        disp(['Result: ' result]);
        warning('Simatra:simCheckVersion', ['simEngine did not execute properly']);
    end
    result = false;
end

end
