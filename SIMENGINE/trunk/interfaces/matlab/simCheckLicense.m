% SIMLICENSECHECK
%  Performs license related tasks in simEngine
%
% Usage
%
%  View the current license and status
%   SIMLICENSECHECK
%
%  Create a prompt to add the license or add based on the filename
%   SIMLICENSECHECK('-add'[, <filename>]) 
%
%  Verify that a particular license is active
%   SIMLICENSECHECK('-check'[, <filename>])
%
% Copyright 2010 Simatra Modeling Technologies
%
function [varargout] = simCheckLicense(varargin)

% Process command line arguments
opts = processCommandLineArguments(varargin);

% Check output arguments
if 1 < nargout 
  error('Simatra:simCheckLicense', ['Only one output argument is ' ...
                      'supported']);
end
  
quiet = true;

% if in view mode, pull out the first license
if strcmp(opts.mode, 'view')
  json = execSimEngine('LF licenseToJSON()', quiet);
  if json
    license = JSONToStruct(json);
  else
    error('Simatra:simCheckLicense', 'No valid license for simEngine found');
  end
elseif strcmp(opts.mode, 'check')
  % Read license file
  if isempty(opts.filename)
    license = parseLicense();
  else % now we have a filename
    json = execSimEngine('LF licenseToJSON()', quiet, ['--license-file '...
                         opts.filename]);
    if json
      license = JSONToStruct(json);
    else
      error('Simatra:simCheckLicense', ['%s is not in a valid simEngine '...
                          'license format'], opts.filename);
    end
  end
elseif strcmp(opts.mode, 'add')
  % Don't do anything yet
end



% Return the license if there's an output argument
if 1 == nargout
  varargout{1} = license;
end


end

% process all the command line arguments
function opts = processCommandLineArguments(cmd_args)

% default values
opts = struct('internal', false,...
              'mode', 'view',... % view means to see the current license
              'filename', '');


if length(cmd_args) > 2
  error('Simatra:simCheckLicense', ['Run help on simCheckLicense for '...
        'proper usage']);
end

if length(cmd_args) >= 1
  if strcmp(cmd_args{1}, '-SIMATRAINTERNALCOMMAND!!!')
    opts.internal = true;
  elseif strcmpi(cmd_args{1}, '-add')
    opts.mode = 'add';
  elseif strcmpi(cmd_args{1}, '-check')
    opts.mode = 'check';
  else
    error('Simatra:simCheckLicense', ['Invalid first argument ''%s'''], ...
          cmd_args{1});
  end
  if length(cmd_args) == 2
    if exist(cmd_args{2}, 'file')
      opts.filename = cmd_args{2};
    else
      error('Simatra:simCheckLicense', ['Second argument must be a filename']);
    end
  end
else
  % just use default options
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
json = execSimEngine(['LF licenseToJSON(' lic_str ')'], quiet);
if json
  license = JSONToStruct(json);
else
  license = false;
end

end


% Grab the license from the command line
function license = parseLicense()

license = false;

ATEND = @(str)(not(isempty(regexp(str, '.*==$'))));
RETURN = @(str)(isempty(str));

str = '';

disp(['Please copy and paste the license data after this prompt.  The '...
     'license should be 6 lines long and end with two ''='' signs. Otherwise, ' ...
     'press <return> to exit out']);

while true()
  linecount = 0;
  str_array = {};
  while linecount == 0 || (not(ATEND(str)) && not(RETURN(str)) && linecount <= 6)
    linecount = linecount + 1;
    if 1 == linecount 
      str = input('Copy License Here: ', 's');
    else
      str = input('', 's');
    end
    str_array{linecount} = str;
  end

  if linecount > 6
    warning('Simatra:simCheckLicense', ['License should only be 6 lines '...
                        'long.  If you don''t have a license available, please press <return>' ...
                        ' to exit.'])
  end

  if ATEND(str)
    if linecount < 6 
      warning('Simatra:simCheckLicense', ['License should only be 6 lines '...
                          'long.  If you don''t have a license available, please press <return>' ...
                          ' to exit.'])
    else
      % it's all good now
      license = evaluateLicense(str_array, true);
      if isstruct(license)
        break;
      else
        warning('Simatra:simCheckLicense', ['License is not a valid '...
                            'license.  Please re-enter license or '...
                            'press <return> to exit.']);
      end
    end
  end
  
  if RETURN(str)
    % break here
    license = false;
    break;
  end
  disp(' ');
end
end


% Run simEngine command and return result
function result = execSimEngine(cmd, quiet, varargin)

if nargin > 2
  additional_options = varargin{1};
else
  additional_options = '';
end

[pathstr, name, ext] = fileparts(which('simex'));
simEngine = fullfile(pathstr, 'bin', 'simEngine');
print_cmd = ['println(' cmd ')'];
echo_cmd = ['echo "' print_cmd '"'];
arguments = [' -batch - -startupmessage=false ' additional_options];

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

if status ~= 0 || ~isempty(strfind(result, 'ERROR')) || ~isempty(strfind(result, 'FAILURE'))
    if not(quiet)
        disp(['Command: ' full_cmd]);
        disp(['Status: ' num2str(status)]);
        disp(['Result: ' result]);
        warning('Simatra:simCheckVersion', ['simEngine did not execute properly']);
    end
    result = false;
end

end


% Convert the JSON code to a structure
function s = JSONToStruct(json)

if isstr(json)
  try 
    s = parse_json(json);
  catch
    error('Simatra:simCheckLicense', 'Can not parse: %s', json);
  end
else
    error('Simatra:simCheckLicense', 'JSON Input is not a string');
end
end