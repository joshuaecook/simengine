% INSTALL Installs the Simatra Toolkit into the Matlab installation
% and updates the search path
function install_wrapper()

% check octave
using_octave = in_octave;

% ask to install
disp(' ')
ret = ask_yn('Install simEngine by Simatra', 'y');
if not(ret)
  return
end

% accept license agreement
disp(' ')
disp('Displaying license agreement ...')
disp('...')
disp('...')
disp('...')
disp('...')
disp('...')
disp(' ')

ret = ask_yn('Accept license agreement');
if not(ret)
  return
end

% check for installation path
disp(['By default, the simEngine platform is installed as a toolbox ' ...
      'within Matlab.'])

if using_octave 
  default_install_path = [octave_config_info.datadir '/octave/packages'];
else
  default_install_path = sprintf('%s/toolbox',getenv('MATLAB'));
end
dir = ask_dir('Matlab installation path', default_install_path);

topinstalldir = 'Simatra';

% check if there is already an installation
target_dir = sprintf('%s/%s', dir, topinstalldir);
if exist(target_dir, 'dir')
  if ask_yn(sprintf('Installation already exists, remove ''%s'' and recreate',target_dir))
    clear('buildEngine');
    clear('simEngine_wrapper');
    if using_octave
      prev_setting = confirm_recursive_rmdir;
      confirm_recursive_rmdir(false);
    end
    [success, message, messageid] = rmdir(target_dir, 's');
    if using_octave
      confirm_recursive_rmdir(prev_setting);
    end
    if not(success) 
      disp(sprintf(['Installer was unable to remove target directory.  ' ...
                    ' Please manually remove ''%s'''],target_dir))
      error('simatra:installError',sprintf('%s (%s)',message,messageid));
      return
    end
  else
    disp('Restart installer to set a new installation directory');
    return
  end
end

% create an installation directory
[success, message, messageid] = mkdir(dir, topinstalldir);
if not(success)
  disp(sprintf('Couldn''t create directory "%s/%s"',dir, topinstalldir));
  error('simatra:installError', sprintf('%s (%s)', message, messageid));
  return
end

% pull out encapsulated tgz file
disp('Installing simEngine files ...');
tgz_file = saveBinaryFile();
installdir = sprintf('%s/%s', dir, topinstalldir);
untar(tgz_file, installdir);


% build the simEngine_wrapper (ANSI-C so no options required)
disp('Building architecture specific mex wrappers ...')
include_dir = fullfile(installdir, 'include');
src_file = fullfile(installdir, 'src', 'simEngine_wrapper.c');
mex_file = fullfile(installdir, 'simEngine_wrapper');
compile_mex(src_file, mex_file, include_dir);
src_file = fullfile(installdir, 'src', 'simex_helper.c');
mex_file = fullfile(installdir, 'simex_helper');
compile_mex(src_file, mex_file, include_dir);

% now add to the path
if ask_yn('Add functions to the default search path')
  addpath(installdir)
  savepath  
elseif ask_yn('Add to the current path')
  addpath(installdir);
end

% now rehash
if not(using_octave)
  rehash('toolbox');
else
  rehash
end

% clean up the generated files
delete(tgz_file);

% complete
disp(' ')
disp('Congratulations!  Simatra simEngine is now installed.')
disp('Use the buildEngine command to compile DSL models.')
disp(['Find example models in ' target_dir '/examples'])
disp(' ')

end

% compile mex files from source to destination
function compile_mex(src_file, mex_file, include_dir)

switch computer
 case 'GLNX86'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m32 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-ldl', '-lgomp', '-output', mex_file, ['-I' include_dir], src_file);
 case 'GLNXA64'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m64 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-ldl', '-lgomp', '-output', mex_file, ['-I' include_dir], src_file);
 case 'MACI'
  mex('-ldl', '-lgomp', '-output', mex_file, ['-I' include_dir], src_file);
 case 'MACI64'
  mex('-ldl', '-lgomp', '-output', mex_file, ['-I' include_dir], src_file);
 case 'i686-pc-linux-gnu'
  mex('-ldl', '-lgomp', '--output', [mex_file '.mex'], ['-I' include_dir], src_file);
 otherwise
  error('Simatra:PlatformError', 'Unsupported platform');
end


end


% check if in octave
function y = in_octave

y = exist('octave_config_info');

end

% decode the included hex file into a restored file
function decode_hex(str, filename)

%disp(sprintf('Calling decode_hex with file=%s',filename));

hex = reshape(str, 2, length(str)/2)';
data = hex2dec(hex);
[fid, message] = fopen(filename,'w+');
if fid==-1
  error('Simatra:decodeInstallerPackage', message);
  return
end
%disp(sprintf('fid=%d',fid));
fwrite(fid, data);
fclose(fid);

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
  ret = input(input_str,'s');
  if strcmp(ret,'') && ischar(abspath(default)) && exist(abspath(default), 'dir') 
    answer = abspath(default);    
    break;
  elseif ischar(abspath(ret)) && exist(abspath(ret), 'dir')
    answer = abspath(ret);
    break;
  end
  disp(['Please enter in a directory that currently exists and can ' ...
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
  ret = input(input_str,'s');
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

% determine the absolute path if there is a relative path
function full = abspath(p)

% by default, return false
full = false;

if isPrefix(p, '/') % full path
  full = p;
elseif isPrefix(p, '~/') % home directory
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
  ret = input(input_str,'s');
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