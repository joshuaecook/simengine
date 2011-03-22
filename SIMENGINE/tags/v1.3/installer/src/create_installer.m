% CREATE_INSTALLER - builds install.m
%
% Usage:
%    create_installer(VERSION [, BUILD])
%
% Examples
%    create_installer('1.2') % for a tag build
%    create_installer('1.2', 4042) % for a dev build
%
% Copyright 2009-2011 Simatra Modeling Technologies
%
function create_installer(input_file)

wrapper = 'install_wrapper.m';
install_path = '../';
if ~exist(input_file, 'file')
    error('Simatra:create_installer', 'Input file <%s> does not exist', input_file);
end
[filepath, filename, fileext] = fileparts(input_file);
filename = regexprep(filename, '[\.-]', '_');
save_file = [filename '.m'];
save_pfile = [filename '.p'];
if strcmpi(fileext, 'tgz')
    error('Simatra:create_installer', 'Expecting input file to be a tgz file');
end
output_file = [fullfile(install_path, filename) '.m'];

% add the license file
filelist = struct('file', 'license.txt', 'fcnname', ...
    'get_license', 'bin', false);

% verify testing directory
if ~exist('../../testing')
  warning('Simatra:create_installer', ['No testing directory available, '...
          'will not add tests to the installer']);
else
  !ln -fs ../../testing
  createSelfExtracting('../simatraRunTests.m','testing/AllTests',{'testing'},...
                       'out.Execute(varargin{:})',...
                       'out.writeXML(fullfile(cwd, ''bamboolog.xml''))',...
                       'out = [out.Total out.Passed out.Failed out.Errored out.Skipped]');
  filelist(length(filelist)+1) = struct('file', '../simatraRunTests.m', 'fcnname', ...
                                        'get_tests', 'bin', true);
end

% finally, add the installer tgz files
type = computer;
filelist(length(filelist)+1) = struct('file', input_file, 'fcnname', ...
    ['restore_' type], 'bin', true);

% append all the files to the wrapper
append_files(wrapper, save_file, filelist);
movefile(save_file, output_file, 'f');
cwd = pwd;
cd(install_path);
pcode(save_pfile);
cd(cwd);
disp(' ');
disp(['Generated new installer at ' fullfile(install_path,save_pfile)]);


end
