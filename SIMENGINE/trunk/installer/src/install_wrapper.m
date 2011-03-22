% INSTALL Installs the Simatra Toolkit into the Matlab installation
% and updates the search path
function install_wrapper(varargin)

% add a test mode
test_mode = false;

% add an upgrade mode
v = ver;
found_in_ver = false;
for i=1:length(v)
  if strcmp(v(i).Name, 'SIMATRA simEngine')
    found_in_ver = true;
  end
end
found_simex = (exist('simex') == 6); % test if simex is found and is
                                     % a p-file
isUpgrade = found_in_ver && found_simex;

if nargin == 1
    if strcmp(varargin{1},'-test')
        disp(' ');
        disp('****** Running in Test Mode ********')
        test_mode = true;
%    elseif strcmpi(varargin{1},'-upgrade')
%      disp(' Upgrading from previous installation')
%      upgrade_mode = true;
    else
        error('Simatra:simEngine:install', 'No arguments expected for the installer');
    end
elseif nargin > 1
    error('Simatra:simEngine:install', 'No arguments expected for the installer');
end

% check octave
using_octave = in_octave;
if using_octave
  disp_wrap(['Error: Simatra simEngine does not support Octave at this ' ...
             'time.  We are currently supporting Matlab on both the ' ...
             'Linux and Mac platforms.'])
  return
end

% ask to install
disp(' ')
disp(['Simatra simEngine installer for MATLAB(TM)'])
disp(['(Copyright 2006-2011 Simatra Modeling Technologies, '...
      'L.L.C.)'])

% ======= CHECKING COMPUTER ========
% check to see if there is a version to install
fprintf(1, 'Checking for a supported architecture: ');
switch computer
    case {'MACI','MACI64'}
        restore_filename = ['restore_' computer];
    case {'GLNX86', 'GLNXA64'}
        restore_filename = ['restore_' computer];
    case {'PCWIN','PCWIN64'}
        disp_wrap(['Error: Simatra simEngine is not currently supported on ' ...
                   'the Microsoft Windows platform.  Please contact us at ' ...
                   'support@simatratechnologies.com if you would like to ' ...
                   'request that a Windows port be supported.'])
        return;
    case 'SOL64'
        disp_wrap(['Error: Simatra simEngine is not currently supported on ' ...
                   'the Solaris platform.  Please contact us at ' ...
                   'support@simatratechnologies.com if you would like to ' ...
                   'request that a Solaris port be supported.'])
        return;
    otherwise
        disp_wrap(['Error: Simatra simEngine is not currently supported on this '...
                   'platform (' computer ').  Please contact us at support@simatratechnologies.com '...
                   'for information on when we will be able to support your ' ...
                   'platform.'])
        return;
end
disp_wrap(['Supporting ' computer]);


% ======= CHECKING DEPENDENCIES ========
disp(' ');
disp_wrap('simEngine installer will now check dependencies')

%%% check if local/global install
isLocal = true; % assume it's local, but test if there's a global installation
doUpgrade = false;
%%% check to see if the user wants to upgrade
if isUpgrade
    try
        % Test this line, since it might throw an exception if Simatra
        % doesn't exist
        global_path = toolboxdir('Simatra');
        % now we know it's global since the above command survived
        isLocal = false;
        default_install_path = global_path;
    catch
        % we know it will throw an exception here if it doesn't exist
        [default_install_path] = fileparts(which('simex'));
    end
    doUpgrade = ask_yn(sprintf('An existing installation was found in %s. Would you like to upgrade?', default_install_path), 'y');
end

if ~isUpgrade || ~doUpgrade
  % determine the toolbox directory for new installations
  if using_octave 
    error('Simatra:installer', 'simEngine is supported only in MATLAB');
  else
    default_install_path = sprintf('%s/toolbox',getenv('MATLAB'));
  end
  % Verify that this directory is writable and we can install there
  capableGlobalInstall = dirIsWritable(default_install_path);
    
  % Assuming we can (likely not possible on Linux or network installs)
  if (capableGlobalInstall)
    isLocal = not(ask_yn(' -> Do you wish to install for all users in the MATLAB toolbox directory'));
  end
  if (isLocal)
    default_install_path = abspath('~');
  end
  default_install_path = fullfile(default_install_path, 'Simatra');
end

  
% check for installation path
topinstalldir = 'Simatra';
if doUpgrade
  disp_wrap(['Overwriting existing installation in ' ...
             default_install_path]);
  target_dir=default_install_path;
  [directory] = fileparts(default_install_path); % strip off
                                                 % trailing Simatra
else
  disp_wrap(['By default, the simEngine platform is installed as a toolbox ' ...
             'within Matlab.  A subdirectory with name ''Simatra'' will be '...
             'created in the installation directory.'])
  [install_path] = fileparts(default_install_path); % strip off
                                                    % trailing Simatra
  directory = ask_dir(' -> Simatra installation directory', install_path);
  target_dir = fullfile(directory, topinstalldir);
end



%%% untar to tmp
tmp = tempname;
[success] = mkdir(tmp);
if ~success 
  error('Simatra:installError', ['Can''t create a temporary directory '...
        'in ''' tempdir ''' to stage the installation.']);
end

% pull out encapsulated tgz file
disp('Extracting simEngine files ...');
tgz_file = feval(restore_filename);
tmpinstalldir = sprintf('%s/%s', tmp, topinstalldir);
untar(tgz_file, tmp);

installdir = fullfile(directory, topinstalldir);


%%% run simEngine to check deps

[status, output] = system(sprintf('%s/bin/simEngine -depcheck', tmpinstalldir));
result = status == 0;


%%% verify deps
if ~result
  disp(' ');
disp(output);
  disp_wrap(['Not all dependencies have been met.  Please contact ' ...
             'support@simatratechnologies.com or check the forums at ' ...
             'www.simatratechnologies.com/forum to assist you in ' ...
             'resolving the dependency issues.']);
  disp(' ');
  ret = ask_yn([' -> Would you like to still continue on with the ' ...
                'installation of simEngine (it is likely that simEngine ' ...
                'will not work correctly)'], 'n');
  if not(ret)
    return
  end
else

  % % Continue on with the install
  % disp(' ');
  % disp('All system requirements have been satisfied.');
  % ret = ask_yn(' -> Would you like to install simEngine by Simatra', 'y');
  % if not(ret)
  %   return
  % end
end


% check if there is already an installation
if exist(target_dir, 'dir')
  
  %%% check permission on path def

  %check if target_dir has good permissions
  
  if isUpgrade || ask_yn(sprintf(' -> Installation already exists, remove ''%s'' and recreate',target_dir))
    clear('simex');
    clear('simplot');
    if using_octave
      prev_setting = confirm_recursive_rmdir;
      confirm_recursive_rmdir(false);
    end
    if isInPath(path, target_dir)  % just in case this exists, we
                                   % remove it so we don't get a warning
      rmpath(target_dir);
    end
    [success, message, messageid] = rmdir(target_dir, 's');
    if using_octave
      confirm_recursive_rmdir(prev_setting);
    end
    if not(success) 
      disp_wrap(sprintf(['Installer was unable to remove target directory.  ' ...
                    ' Please manually remove ''%s'''],target_dir))
      error('Simatra:installError','%s (%s)',message,messageid);
    end
  else
    disp_wrap('Restart installer to set a new installation directory');
    return
  end
end



%%% make install dir (if new)
% create an installation directory
[success, message, messageid] = mkdir(directory, topinstalldir);
if not(success)
  disp(sprintf('Couldn''t create directory "%s/%s"',directory, topinstalldir));
  error('Simatra:installError', '%s (%s)', message, messageid);
end

%%% copy tmp dir to install dir
try
  copyfile(tmpinstalldir, installdir);
catch
  error('Simatra:installError', ['Can not copy files into directory ''' installdir ''''])
end
[success] = rmdir(tmpinstalldir,'s'); % not worth a warning if it fails

% copy both 'simcheck' and 'simtest' to installdir
disp_wrap('Copying additional files from installer ...');
restored_filename = get_tests();
simtest_file = fullfile(installdir, 'simatra_simtest.p');
copyfile(restored_filename,simtest_file);
delete(restored_filename);

%%% update pathdef if global
if (not(isLocal))
  % Check if path is in global pathdef run at startup
  if ~isInPath(pathdef, installdir) && ask_yn([' -> Add simEngine ' ...
                        'to your default search path'])
      save_p = path;
      path(pathdef);
      addpath(installdir);
      try
        savepath;
      catch
        % this should never get here since we should already check the
        % permissions
      end
      path(save_p);
      addpath(installdir);
  else
    % already in current working path
    if ~isInPath(path, installdir)
      addpath(installdir);
    end
  end

elseif ask_yn(' -> Add simEngine to your default search path')
  %%% append path to startup.m if local
  x = regexp(userpath, ':', 'split');
 
  userpathdir = x{1};
    
  addpath(installdir);

  if (isdir(userpathdir))
    iswritable = dirIsWritable(userpathdir);
    
    if (iswritable)
      startupfile = fullfile(userpathdir, 'startup.m');
      if (exist(startupfile, 'file'))
        % concatenate
        writeStartupCode(startupfile, 'SIMENGINESTARTUP', 'simEngineStartup', 'simEngineStartup');
      else
        % create a new one!
        fid = fopen(startupfile, 'w');
        fclose(fid);
        writeStartupCode(startupfile, 'SIMENGINESTARTUP', 'simEngineStartup', 'simEngineStartup');
      end
    else
      %create a userpath dir, reset user path, add old userpath
      %using addpath in NEW startup.m
      if(not(isdir('~/.simatra')))
        mkdir('~/.simatra');
      end
      
      userpath('reset');
      userpath('~/.simatra');

      startupfile = '~/.simatra/startup.m';
      
      try
        fid = fopen(startupfile, 'w');
        fprintf(fid, 'addpath(''%s'');', userpathdir);
        fclose(fid);
      catch
        warning('Simatra:installer', ['Can''t open file ''' startupfile ...
                            ''' for write'])

      end
      if fid >= 0
        writeStartupCode(startupfile, 'SIMENGINESTARTUP', ...
                         'simEngineStartup', 'simEngineStartup');
      end      
      
    end
  else
    %create a userpath dir and reset user path and add new startup.m
      if(not(isdir('~/.simatra')))
        mkdir('~/.simatra');
      end
      
      cwd = cd('~');
      homedir = pwd;
      cd(cwd);
      if isdir('~/Documents')
        userpath('reset');
      end
      userpath(fullfile(homedir, '.simatra'));

      startupfile = '~/.simatra/startup.m';
      
      try
        fid = fopen(startupfile, 'w');
        fclose(fid);
      catch
        warning('Simatra:installer', ['Can''t open file ''' startupfile ...
                ''' for write'])
      end
      if fid >= 0
        writeStartupCode(startupfile, 'SIMENGINESTARTUP', ...
                         'simEngineStartup', 'simEngineStartup');
      end
  end
    
  %%% add path
  tag = sprintf('ADDPATH%s', installdir);
  if ~tagExists(startupfile, tag) % && ask_yn(' -> Add functions to the default search path')
    writeStartupCode(startupfile, tag, false, sprintf('addpath(''%s'');', installdir));    
    addpath(installdir);
  %elseif ~isInPath(path, installdir) && ask_yn(' -> Add to the current path')
  %  addpath(installdir);
  end
  rehash

end

% now rehash
if not(using_octave)
  rehash('toolbox');
else
  rehash
end

%%% clean up the generated files
delete(tgz_file);
  

%%% if we're in a test mode, we can run the system tests
if test_mode
    if ask_yn(' -> Execute simEngine system tests to verify functionality')
        test_filename = get_tests();
        [testpath,testfcn,textext] = fileparts(test_filename);
        results = feval(testfcn, '-release');
        total = results(1);
        passed = results(2);
        if total==passed
            disp_wrap('SUCCESS - all tests passed');
        else
            disp_wrap(sprintf('FAILURE - %d/%d tests passed', passed, total));
        end
    end
end

%%% complete
disp(' ')
disp_wrap(['Congratulations!  Simatra simEngine is now installed.'])

disp(' ')
disp_wrap(['Before using simEngine, please restart MATLAB so that' ...
           ' all of simEngine is in the path.'])
disp(' ')
disp_wrap(['Then, browse for simEngine by running ''doc'' and start working '...
           'on a simEngine tutorial, or browse some of the demo files.'])
disp(' ')
disp_wrap(['  For questions or comments, please contact us at ' ...
           'support@simatratechnologies.com or post at ' ...
           'http://groups.google.com/group/simengine.'])
disp(' ')


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%%% Helper Functions


% check if in octave
function y = in_octave

y = exist('octave_config_info');

end

% decode the included hex file into a restored file
function [varargout] = decode_hex(str, varargin)

%disp(sprintf('Calling decode_hex with file=%s',filename));

hex = reshape(str, 2, length(str)/2)';
data = hex2dec(hex);
if nargin == 2
    [fid, message] = fopen(varargin{1},'w+');
    if fid==-1
        error('Simatra:decodeInstallerPackage', message);
        return
    end
    fwrite(fid, data);
    fclose(fid);
elseif nargout==1
    varargout{1} = char(data');
end

end


% ask_dir(str) asks a question for the user to enter in a directory
% name, then will check to make sure that it is acceptable
% It will then return that directory
function answer = ask_dir(str, varargin)

default = '';
input_str = sprintf('%s? ', str);
if nargin == 2 
  default = varargin{1};
  input_str = sprintf('%s? (default:%s) ', str, default);
end

while(1),
  ret = input([disp_wrap(input_str) ' '],'s');
  if strcmp(ret,'') && ischar(abspath(default)) && exist(abspath(default), 'dir') 
    answer = abspath(default);    
    break;
  elseif ischar(abspath(ret)) && exist(abspath(ret), 'dir')
    answer = abspath(ret);
    break;
  end
  disp_wrap(['Please enter in a directory that currently exists and can ' ...
        'be written to.']);
end

end

% ask_file(str) asks a question for the user to enter in a 
% filename, then will check to make sure that it is acceptable
% It will then return that filename
function answer = ask_file(str, varargin)

default = '';
input_str = sprintf('%s? ', str);
if nargin == 2 
  default = varargin{1};
  input_str = sprintf('%s? (default:%s) ', str, default);
end

while(1),
  ret = input([disp_wrap(input_str) ' '],'s');
  if strcmp(ret,'') && ischar(abspath(default)) && exist(abspath(default), 'file') 
    answer = abspath(default);    
    break;
  elseif ischar(abspath(ret)) && exist(abspath(ret), 'file')
    answer = abspath(ret);
    break;
  end
  disp('Please enter in a filename that currently exists.');
end

end

function answer = ask_word(str, whatisit)

input_str = sprintf('%s? ', str);

while(1),
  answer = input([disp_wrap(input_str) ' '],'s');
  if strcmp(answer,'')
    disp_wrap(sprintf('Please enter a valid %s', whatisit));
  else
    break;
  end
end

end


% determine the absolute path if there is a relative path
function full = abspath(p)

% by default, return false
full = false;

if isPrefix(p, '/') % full path
  full = p;
elseif isPrefix(p, '~') % home directory
  home = getenv('HOME');
  if strcmp(home,'')
    disp(sprintf(['Home directory location in path ''%s'' can not be determined.  ' ...
                  'There is no HOME environmental variable defined.'], ...
                 p))
  else
    full = regexprep(p, '~', home, 'once');    
  end
elseif isPrefix(p, '~') % another user's directory
  disp(sprintf(['Can''t determine location of ''%s''.  Please use an ' ...
                'absolute path.'], p));
else
  full = fullfile(pwd, p);
end

end

function ret = isPrefix(str, pre)

  s = strfind(str, pre);
  ret = (length(s) >= 1 && s(1) == 1);

end
  

% ask_yn(str) asks a question to the user.  The function
% returns one or zero depending if the answer is yes or no
function answer = ask_yn(str, varargin)

default = '';
input_str = sprintf('%s? (y/n) ', str);
if nargin == 2 
  default = varargin{1};
  input_str = sprintf('%s? (default:%s) ', str, default);
end

while(1),
  ret = input([disp_wrap(input_str) ' '],'s');
   switch lower(ret)
    case '',
     switch default
      case {'y','yes'},
       answer = 1;
       break;
      case {'n', 'no'},
       answer = 0;
       break;
     end
    case {'y','yes'},
     answer = 1;
     break;
    case {'n', 'no'},
     answer = 0;
     break;
   end
   disp('Please respond <yes> or <no>');
end

end

% DISP_WRAP line wrap printer
% Usage:
%   DISP_WRAP(STRING)
%   DISP_WRAP(STRING, COLS)
%
% Copyright 2010 Simatra Modeling Technologies
%
function [varargout] = disp_wrap(str, varargin)

% set the default number of columns
try
    size = get(0, 'CommandWindowSize');
    cols = size(1) - 3; % Subtract a few as a buffer
    rows = size(2);
catch
    cols = 80;
end

% otherwise, use the number of columns passed into p
if nargin > 2
    error('Simatra:p', 'Only two arguments supported to p')
elseif nargin == 2
    cols = varargin{1};
end

% split the string up into lines
lines = strread(str, '%s', 'delimiter', sprintf('\n'));

% perform the wrapping
match = ['(.{1,' num2str(cols) '})(\s+)'];
wrapped_lines = cell(length(lines),1);
for i=1:length(lines)
    if isempty(lines{i})
        wrapped_lines{i} = sprintf('\n');
    else
        wrapped_lines{i} = [strtrim(regexprep([lines{i} ' '], match, '$1\n')) sprintf('\n')];
    end
end
wrapped_str = strtrim([wrapped_lines{:}]);

% if there's an output argument, save the output, otherwise display it
if nargout == 1
    varargout{1} = wrapped_str;
else
    disp(wrapped_str)
end

end

function r = run(cmd)
  [status, result] = system(cmd);
  r = status;
end

function writable = dirIsWritable(path)
  writablefilename = tempname(path);
    
  filehandle = fopen(writablefilename, 'w');
    
  writable = filehandle ~= -1;
    
  if (writable)
    fclose(filehandle);
    delete(writablefilename);
  end
end

function r = tagExists(filename, tag)

if exist(filename, 'file')
  tlines = fileread(filename);
  r = ~isempty(strfind(tlines, tag));
else
  r = false;
end

end

function [] = writeStartupCode(startupfile, tag, funcname, func)
if exist(startupfile, 'file')
    if ~tagExists(startupfile, tag)
        [success, attributes] = fileattrib(startupfile);
        readonly = ~attributes.UserWrite;
        if success && readonly
            success = fileattrib(startupfile, '+w', 'u');
        end        
        if success
            fid = fopen(startupfile,'a');
            if fid == -1
                warning('Simatra:installer', 'Can not open file %s for editing.  Please verify permissions on this file and rerun the installer.', startupfile);
            else
                fprintf(fid, '%% %s - This code was added for simEngine by Simatra Technologies\n', tag);
                if funcname
                    fprintf(fid, 'if (exist(''%s''))\n', funcname);
                    fprintf(fid, '  %s;\n', func);
                    fprintf(fid, 'end\n');
                else
                    fprintf(fid, '  %s;\n', func);
                end
                fclose(fid);
            end
        else
            warning('Simatra:installer', 'Can not update write permissions on %s', startupfile);
        end
        if readonly
            success = fileattrib(startupfile, '-w', 'u');
        end
  end
else
  warning('Simatra:installer', ['Can''t update file ''' startupfile ...
                      ''' with settings'])
end
end


% Checks if directory d is in path p
function r = isInPath(p, d)

directories = regexp(p,':','split');
r = false;
absd = abspath(d);
for i=1:length(directories)
  if strcmpi(abspath(directories{i}), absd)
    r = true;
    break;
  end
end
end