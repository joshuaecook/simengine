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
disp('END USER LICENSE AGREEMENT FOR SIMENGINE™ SOFTWARE');
disp(' ');
disp('IMPORTANT - READ CAREFULLY: This End-User License Agreement ("EULA")');
disp('is a legal agreement between you (either an individual or a single');
disp('entity) and SIMATRA MODELING TECHNOLOGIES, LLC (“SIMATRA”), for the');
disp('SIMENGINE™ software (the “Software”), which includes computer software');
disp('and all associated media, printed materials, and "online" or');
disp('electronic documentation.  By installing, copying, downloading,');
disp('accessing, or otherwise using the Software, you agree to be bound by');
disp('the terms of this EULA. If you do not agree to the terms of this EULA,');
disp('do not install or use the Software.');
disp(' ');
disp('1.      The Software is protected by copyright laws and international');
disp('copyright treaties and other intellectual property laws and treaties.');
disp('The Software is licensed, not sold.');
disp(' ');
disp('2.      As long as you comply with the terms of the EULA and Amendment,');
disp('SIMATRA grants to you a non-exclusive license to use the Software on');
disp('single computer or workstation at one time except each user registered');
disp('with valid user identification is permitted to use the Software');
disp('through a home personal computer or another personal computer provided');
disp('that use is limited to one personal computer at a time.');
disp(' ');
disp('3.      You may also store or install a copy of the Software on a network');
disp('server, used only to run the Software on your other computers over an');
disp('internal network; however, you must first acquire and dedicate a');
disp('license for each separate computer on which the Software is run from');
disp('the storage device. A license for the Software may not be shared or');
disp('used concurrently on different computers.');
disp(' ');
disp('4.      You may make one backup copy of the Software, provided your backup');
disp('copy is not installed or used on any computer.  You may not transfer');
disp('the rights to a backup copy unless you transfer all rights in the');
disp('Software.');
disp(' ');
disp('5.      The Software is licensed as a single product. Its component parts');
disp('may not be separated for use on more than one computer.');
disp(' ');
disp('6.      You may not rent, lease, or lend the Software or use the Software');
disp('as part of a service bureau.');
disp(' ');
disp('7.      All rights not expressly granted are reserved by SIMATRA.');
disp(' ');
disp('8.      The Software may contain links to other World Wide Web sites or');
disp('resources. Because SIMATRA has no control over such sites and');
disp('resources, you acknowledge and agree that SIMATRA is not responsible');
disp('for the availability of such external sites or resources, and does not');
disp('endorse and is not responsible or liable for any content, advertising,');
disp('products, or other materials on or available from such sites or');
disp('resources. You further acknowledge and agree that SIMATRA shall not be');
disp('responsible or liable, directly or indirectly, for any damage or loss');
disp('caused or alleged to be caused by or in connection with use of or');
disp('reliance on any such content, goods or services available on or');
disp('through any such site or resource.');
disp(' ');
disp('9.      Except as expressly provided herein, SIMATRA licenses the Software');
disp('to Licensee on an "AS IS" basis.');
disp(' ');
disp('10.     THE ABOVE WARRANTIES ARE EXCLUSIVE AND ARE IN LIEU OF ALL OTHER');
disp('WARRANTIES OR CONDITIONS, EXPRESS, IMPLIED OR STATUTORY.  SIMATRA');
disp('MODELING TECHNOLOGIES AND ITS SUPPLIERS SPECIFICALLY DISCLAIM ALL');
disp('OTHER WARRANTIES AND CONDITIONS, EITHER EXPRESS OR IMPLIED, INCLUDING');
disp('BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, FITNESS, fitness for');
disp('a particular purpose, or suitability of use ALONE OR IN CONNECTION');
disp('WITH LICENSEE’S CONTENT OR ANY THIRD PARTY CONTENT ACCESSED THROUGH');
disp('THE SOFTWARE, USE IN ANY TRADE OR PROFESSION, TITLE AND');
disp('NON-INFRINGEMENT, WITH REGARD TO THE SOFTWARE.');
disp(' ');
disp('11.     The Software is not intended to be a substitute for the exercise');
disp('of professional judgment.');
disp(' ');
disp('12.     SIMATRA and its suppliers’ entire liability and your exclusive');
disp('remedy shall be, at SIMATRA’S option, either (a) return of the price');
disp('paid, if any, or (b) repair or replacement of the Software.');
disp(' ');
disp('13.     To the maximum extent permitted by applicable law, SIMATRA and its');
disp('suppliers disclaim all other warranties and conditions. This limited');
disp('warranty gives you specific legal rights. You may have others, which');
disp('vary from state/jurisdiction to state/jurisdiction.');
disp(' ');
disp('14.     To the maximum extent permitted by applicable law, in no event');
disp('shall SIMATRA or its suppliers be liable for any special, incidental,');
disp('indirect, or consequential damages whatsoever (including, without');
disp('limitation, damages for loss of business profits, business');
disp('interruption, loss of business information, or any other pecuniary');
disp('loss) arising out of the use of or inability to use the Software or');
disp('the provision of or failure to provide support services, even if');
disp('SIMATRA has been advised of the possibility of such damages. In any');
disp('case, SIMATRA’S entire liability under any provision of this EULA');
disp('shall be limited to the amount actually paid in license fees.  Because');
disp('some states and jurisdictions do not allow the exclusion or limitation');
disp('of liability, the above limitation may not apply to you.');
disp(' ');
disp('15.     Upon request by SIMATRA, not more than once annually, you will');
disp('report the number of copies in use by you.');
disp(' ');
disp('16.     If you acquired this Product in the United States, this EULA is');
disp('governed by the laws of the State of Georgia.');
disp(' ');
disp(' ');
disp('All uses of the SUNDIALS suite is ');
disp('Copyright 2002, The Regents of the University of California.');
disp('Produced at the Lawrence Livermore National Laboratory.');
disp('All rights reserved.');
disp(' ');
disp(' ');
disp('IMPORTANT - READ CAREFULLY: This End-User License Agreement ("EULA")')
disp('is a legal agreement between you (either an individual or a single')
disp('entity) and SIMATRA MODELING TECHNOLOGIES, for the SIMENGINE software')
disp('(the "Software"), which includes computer software and may include')
disp('associated media, printed materials, and "online" or electronic')
disp('documentation.  By installing, copying, downloading, accessing, or')
disp('otherwise using the Software, you agree to be bound by the terms of')
disp('this EULA. If you do not agree to the terms of this EULA, do not')
disp('install or use the Software.')
disp(' ')
disp('1. The Software is protected by copyright laws and international')
disp('copyright treaties and other intellectual property laws and treaties.')
disp('The Software is licensed, not sold.')
disp('2. As long as you comply with the terms of the EULA and Amendment,')
disp('SIMATRA MODELING TECHNOLOGIES grants to you a non-exclusive license to')
disp('use the Software on single computer or workstation at one time except')
disp('each user registered with valid user identification is permitted to')
disp('use the Software through a home personal computer or another personal')
disp('computer provided that use is limited to one personal computer at a')
disp('time.')
disp('3. You may also store or install a copy of the Software on a network')
disp('server, used only to run the Software on your other computers over an')
disp('internal network; however, you must first acquire and dedicate a')
disp('license for each separate computer on which the Software is run from')
disp('the storage device. A license for the Software may not be shared or')
disp('used concurrently on different computers.')
disp('4. You may make one backup copy of the Software, provided your backup')
disp('copy is not installed or used on any computer.  You may not transfer')
disp('the rights to a backup copy unless you transfer all rights in the')
disp('Software.')
disp('5. The Software is licensed as a single product. Its component parts')
disp('may not be separated for use on more than one computer.')
disp('6. You may not rent, lease, or lend the Software or use the Software')
disp('as part of a service bureau.')
disp('7. All rights not expressly granted are reserved by SIMATRA MODELING')
disp('TECHNOLOGIES.')
disp('8. The Software may contain links to other World Wide Web sites or')
disp('resources. Because SIMATRA MODELING TECHNOLOGIES has no control over')
disp('such sites and resources, you acknowledge and agree that SIMATRA')
disp('MODELING TECHNOLOGIES is not responsible for the availability of such')
disp('external sites or resources, and does not endorse and is not')
disp('responsible or liable for any content, advertising, products, or other')
disp('materials on or available from such sites or resources. You further')
disp('acknowledge and agree that SIMATRA MODELING TECHNOLOGIES shall not be')
disp('responsible or liable, directly or indirectly, for any damage or loss')
disp('caused or alleged to be caused by or in connection with use of or')
disp('reliance on any such content, goods or services available on or')
disp('through any such site or resource.')
disp('9. SIMATRA MODELING TECHNOLOGIES warrants that the media on which')
disp('copies of the Software is delivered to Licensee will be free from')
disp('defects in materials and workmanship for a period of 90 days after')
disp('delivery.  Except as expressly provided above, SIMATRA MODELING')
disp('TECHNOLOGIES licenses the Software to Licensee on an "AS IS" basis.')
disp('10. THE ABOVE WARRANTIES ARE EXCLUSIVE AND ARE IN LIEU OF ALL OTHER')
disp('WARRANTIES OR CONDITIONS, EXPRESS, IMPLIED OR STATUTORY.  SIMATRA')
disp('MODELING TECHNOLOGIES AND ITS SUPPLIERS SPECIFICALLY DISCLAIM ALL')
disp('OTHER WARRANTIES AND CONDITIONS, EITHER EXPRESS OR IMPLIED, INCLUDING')
disp('BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, FITNESS, fitness for')
disp('a particular purpose, or suitability of use ALONE OR IN CONNECTION')
disp('WITH LICENSEE��S CONTENT OR ANY THIRD PARTY CONTENT ACCESSED THROUGH')
disp('THE SOFTWARE, USE IN ANY TRADE OR PROFESSION, TITLE AND')
disp('NON-INFRINGEMENT, WITH REGARD TO THE SOFTWARE.')
disp('11. The Software is not intended to be a substitute for the exercise')
disp('of professional judgment.')
disp('12. SIMATRA MODELING TECHNOLOGIES and its suppliers'' entire liability')
disp('and your exclusive remedy shall be, at SIMATRA MODELING TECHNOLOGIES')
disp('option, either (a) return of the price paid, if any, or (b) repair or')
disp('replacement of the Software.')
disp('13. To the maximum extent permitted by applicable law, SIMATRA')
disp('MODELING TECHNOLOGIES and its suppliers disclaim all other warranties')
disp('and conditions. This limited warranty gives you specific legal rights.')
disp('You may have others, which vary from state/jurisdiction to')
disp('state/jurisdiction.')
disp('14. To the maximum extent permitted by applicable law, in no event')
disp('shall SIMATRA MODELING TECHNOLOGIES or its suppliers be liable for any')
disp('special, incidental, indirect, or consequential damages whatsoever')
disp('(including, without limitation, damages for loss of business profits,')
disp('business interruption, loss of business information, or any other')
disp('pecuniary loss) arising out of the use of or inability to use the')
disp('Software or the provision of or failure to provide support services,')
disp('even if SIMATRA MODELING TECHNOLOGIES has been advised of the')
disp('possibility of such damages. In any case, SIMATRA MODELING')
disp('TECHNOLOGIES entire liability under any provision of this EULA shall')
disp('be limited to the amount actually paid  in license fees.  Because some')
disp('states and jurisdictions do not allow the exclusion or limitation of')
disp('liability, the above limitation may not apply to you.')
disp('15. Upon request by SIMATRA MODELING TECHNOLOGIES, not more than once')
disp('annually, you will report the number of copies in use by you.')
disp('16. If you acquired this Product in the United States, this EULA is')
disp('governed by the laws of the State of Georgia.')
disp(' ')
disp(' ')


ret = ask_yn('Accept license agreement');
if not(ret)
  return
end

key_file = savePublicKey();
licensefile = ask_file('Enter the license file location');
if not(licensefile)
  disp('You must enter a valid license file')
  return
end

serial = ask_serial('Enter your serial number');

if not(serial)
  disp ('You must enter a valid serial number')
  return
end

key = load(key_file, 'publickey');

if not(verify(licensefile, str2num(serial), key.publickey))
  disp('License and serial do not match.  Please contact Simatra.')
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
tgz_file = saveInstaller();
installdir = sprintf('%s/%s', dir, topinstalldir);
untar(tgz_file, installdir);


% build the simEngine_wrapper (ANSI-C so no options required)
disp('Building architecture specific mex wrappers ...')
src_file = fullfile(installdir, 'src', 'simEngine_wrapper.c');
mex_file = fullfile(installdir, 'simEngine_wrapper');
switch computer
 case 'GLNX86'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m32 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', mex_file, src_file);
 case 'GLNXA64'
  mex('CFLAGS=-std=gnu99 -D_GNU_SOURCE -fPIC -pthread -m64 -fexceptions -D_FILE_OFFSET_BITS=64', ...
      '-output', mex_file, src_file);
 case 'MACI'
  mex('-output', mex_file, src_file);
 case 'MACI64'
  mex('-output', mex_file, src_file);
 case 'i686-pc-linux-gnu'
  mex('--output', [mex_file '.mex'], src_file);
 otherwise
  error('Simatra:PlatformError', 'Unsupported platform');
end

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
delete(key_file);

% complete
disp(' ')
disp('Congratulations!  Simatra simEngine is now installed.')
disp('Use the buildEngine command to compile DSL models.')
disp(['Find example models in ' target_dir '/examples'])
disp(' ')

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

function answer = ask_serial(str)

input_str = sprintf('%s? ', str);

while(1),
  answer = input(input_str,'s');
  if strcmp(answer,'')
    disp('Please enter a valid serial number');
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

function [isValid] = verify(licensefilename, serial, publickey)
try
  sig = java.security.Signature.getInstance('SHA1withRSA'); 
  sig.initVerify(publickey);
  
  license = load(licensefilename, 'license');

  
  sig.update(serial);
  
  isValid = sig.verify(license.license);
catch
  isValid = 0;
end

end
