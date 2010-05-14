% SIMCHECKLICENSE
%  Performs license related tasks in simEngine
%
% Usage
%
%  View the current license and status
%   SIMCHECKLICENSE
%
%  Create a prompt to update the license or add based on the filename
%   SIMLICENSECHECK('-update'[, <filename>]) 
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
    [license, lic_str] = parseLicense();
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
elseif strcmp(opts.mode, 'update')
  % Read license file
  if isempty(opts.filename)
    [license, lic_str] = parseLicense();
    placeLicense(opts, lic_str);
  else % now we have a filename
    json = execSimEngine('LF licenseToJSON()', quiet, ['--license-file '...
                         opts.filename]);
    if json
      license = JSONToStruct(json);
    else
      error('Simatra:simCheckLicense', ['%s is not in a valid simEngine '...
                          'license format'], opts.filename);
    end
    % now read and place the license file
    placeLicense(opts, fileread(opts.filename));
  end
end



% Return the license if there's an output argument
if 1 == nargout && opts.internal
  varargout{1} = license;
elseif isstruct(license)
  displayLicenseInformation(opts, license);
  if 1==nargout 
      varargout{1} = license.version;
  end
end


end

% Print license information
function displayLicenseInformation(opts, license)

disp(' ');
disp('simEngine licensed as the following:')
switch license.version
    case 'DEVELOPMENT'
        disp('  Developer License for simEngine (INTERNAL USE ONLY)');
    case 'BASIC'
        disp('  simEngine Basic Edition');
    case 'TRIAL'
        disp('  simEngine Professional Trial');
        if ~isempty(license.expirationDate) && ~isstruct(license.status)
            disp(sprintf('  Trial expires on %s', license.expirationDate));
        end
        names = fieldnames(license.restriction);
        switch names{1}
            case 'USERNAME'
                disp('  Single User Trial');
            case 'HOSTID'
                disp('  Single Machine Trial');
            case 'LICENSESERVER'
                disp('  Network License Trial');
            case 'SITE'
        end                
    case {'STANDARD','PROFESSIONAL'}
        if strcmp(license.version, 'STANDARD')
            disp('  simEngine Standard Edition');
        else
            disp('  simEngine Professional Edition');
        end
        disp(sprintf('  Licensed to ''%s'' at ''%s''', license.customerName, license.customerOrganization));
        if ~isempty(license.expirationDate) && ~isstruct(license.status)
            disp(sprintf('  Maintenance expires on %s', license.expirationDate));
        end
        names = fieldnames(license.restriction);
        switch names{1}
            case 'USERNAME'
                disp('  Single User License');
            case 'HOSTID'
                disp('  Single Machine License');
            case 'LICENSESERVER'
                disp('  Network License');
            case 'SITE'
                disp(sprintf('  Site Licensed to ''%s''', license.restriction.SITE));
        end                
    otherwise
        error('Simatra:simCheckLicense', 'Unknown licensing scheme encountered')
end
disp(['  ' statusToString(license.status)]);
disp(' ')

end

% statusToString - read the status 
function str = statusToString(status)

if isstruct(status)
    switch status.status
        case 'expired'
            str = sprintf('License has been expired as of %s', status.date);
        case 'outofmaintenance'
            str = sprintf('License has been out of maintenance since %s', status.date);
        case 'invalidversion'
            str = sprintf('License is supported up until version %s (simEngine version is %s)', status.lic_ver, status.cur_ver);
        case 'wronguser'
            if status.queried
                str = 'License is a single-user license assigned to a different user';
            else
                str = 'Current user can not be verified for a single-user license';
            end
        case 'wrongmachine'
            if status.queried
                str = 'License is a single-machine license assigned to a different machine';
            else
                str = 'Current machine can not be verified for a single-machine license';
            end
        case 'networknotsupported'
            str = 'Network licenses are not currently supported';
        otherwise
            error('Simatra:simCheckLicense', 'License is not valid for an unknown reason');
    end
else
    str = 'License is active';
end

end



% Place the license in the directory
function placeLicense(opts, lic)

target_lic_filename = fullfile(opts.licensepath, 'license.key');
if exist(target_lic_filename, 'file')
  % TODO - there is no call in Matlab or Java to determine if
  % something is a symbolic link - have to go into the shell for this
  [status, output] = system(['ls -l ' target_lic_filename]);
  if 'l' == output(1)
    % is link
    delete(target_lic_filename);
  else
    % save file name
    save_filename = fullfile(opts.licensepath, ['license_save' (datestr(now, 'mm-dd-yy_HH:MM:SS')) ...
                        '.key']);
    copyfile(target_lic_filename, save_filename);
    delete(target_lic_filename);
  end
end
new_filename = fullfile(opts.licensepath, ['license' (datestr(now, 'mm-dd-yy_HH:MM:SS')) ...
    '.key']);

% Create a new file name
fid = fopen(new_filename, 'w');
if fid > -1
    fwrite(fid, lic);
    fclose(fid);
else
    error('Simatra:simCheckLicense', 'Can not write into license file %s', new_filename);
end

% Now, create a symbolic link
[status, output] = system(['ln -s ' new_filename ' ' ...
    target_lic_filename]);
if status > 0
    error('Simatra:simCheckLicense', ['Could not create a symbol link '...
        'to %s'], target_lic_filename);
end

end

% process all the command line arguments
function opts = processCommandLineArguments(cmd_args)

% Is it a global install?
globalInstall = ~isempty(strfind(matlabroot, which('simex')));
if globalInstall
  [license_path, name, ext] = fileparts(which('simex'));
else
  license_path = fullfile(getenv('HOME'), '.simatra');
end


% default values
opts = struct('internal', false,...
              'mode', 'view',... % view means to see the current license
              'filename', '', ...
              'licensepath', license_path);

          
if length(cmd_args) >= 1
    if strcmp(cmd_args{1}, '-SIMATRAINTERNALCOMMAND!!!')
        opts.internal = true;
        if length(cmd_args) == 1
            cmd_args = {};
        else
            cmd_args = cmd_args(2:end);
        end
    end
end
          
          
if length(cmd_args) > 2
  error('Simatra:simCheckLicense', ['Run help on simCheckLicense for '...
        'proper usage']);
end

if length(cmd_args) >= 1
  if strcmp(cmd_args{1}, '-SIMATRAINTERNALCOMMAND!!!')
    opts.internal = true;
  elseif strcmpi(cmd_args{1}, '-update')
    opts.mode = 'update';
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
function [license, lic_str] = parseLicense()

% default for the license
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

% concatenate the str_array into a license string
lic_str = '';
for i=1:length(str_array)
  lic_str = sprintf('%s%s\n', lic_str, str_array{i});
end

end


% Run simEngine command and return result
function output = execSimEngine(cmd, quiet, varargin)

if nargin > 2
  additional_options = varargin{1};
else
  additional_options = '';
end

TOKEN = 'STARTING HERE:';
[pathstr, name, ext] = fileparts(which('simex'));
simEngine = fullfile(pathstr, 'bin', 'simEngine');
print_cmd = ['println(\"' TOKEN '\" + ' cmd ')'];
echo_cmd = ['echo "' print_cmd '"'];
arguments = [' -batch - -startupmessage=false ' additional_options];

full_cmd = [echo_cmd ' | ' simEngine arguments];

[status, result] = system(full_cmd);

% chomp the trailing new line
if 10 == double(result(end))
    result = result(1:(end-1));
end

% pull out the last part
last_line = result(findstr(result, TOKEN):end);
output = regexprep(last_line, TOKEN, '');

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