% CREATESELFEXTRACTING creates a self extracting file and runs the top
% function
function createSelfExtracting(filename, top, files, varargin)

temptgz = [tempname '.tgz'];
tar(temptgz,files);

% Create wrapper
tempfile = [tempname '.m'];
fid = fopen(tempfile,'w');
[path, fcnname, ext] = fileparts(filename);
[toppath, topfcnname, ext] = fileparts(top);
if isempty(toppath)
    toppath = '.';
end
cwd = pwd;
cd(toppath);
nargs = nargout(topfcnname);
cd(cwd);
if nargs == 0
    nargout_str = '';
else
    nargout_str = '[';
    for i=1:(nargs-1)
        nargout_str = [nargout_str 'out' num2str(i) ', '];
    end
    nargout_str = [nargout_str 'out' num2str(i+1) '] = '];
end

% just take every additional command and run it after the initial command
% ...
extra_cmd = varargin;

fprintf(fid, 'function %s%s(varargin)\n', nargout_str, fcnname);
fprintf(fid, '  filename = get_included_files();\n');
fprintf(fid, '  fullpath = [pwd ''/'' filename];\n');
fprintf(fid, '  d = tempname;\n');
fprintf(fid, '  mkdir(d);\n');
fprintf(fid, '  cwd = pwd;\n');
fprintf(fid, '  cd(d);\n');
fprintf(fid, '  untar(fullpath);\n');
%fprintf(fid, '  cd(cwd);\n');
%fprintf(fid, '  addpath(d);\n');
fprintf(fid, '  cd(''%s'');\n', toppath);
fprintf(fid, '  rehash\n');
fprintf(fid, '  try\n');
fprintf(fid, '    %s%s(varargin{:});\n', nargout_str, topfcnname);
for i=1:length(extra_cmd)
    fprintf(fid, '    %s;\n', extra_cmd{i});
end
fprintf(fid, '  catch me\n');
fprintf(fid, '    warning(''Simatra:createSelfExtracting'', ''Error caught when running embedded function'')\n');
fprintf(fid, '    cd(cwd);\n');
fprintf(fid, '    rethrow(me)\n');
fprintf(fid, '  end\n');
fprintf(fid, '  cd(cwd);\n');
fprintf(fid, '  rmdir(d, ''s'');\n');
fprintf(fid, '  delete(filename);\n');
fprintf(fid, 'end\n');

fclose(fid);
tempfile2 = [tempname '.m'];
[status, results] = system(['cat ' tempfile ' decode_hex.m > ' tempfile2]);

disp(sprintf('Created self extracting file of ''%s'' for function %s', tempfile2,topfcnname));
append_bin_to_file(temptgz,tempfile2,filename,'get_included_files');

delete(temptgz);
delete(tempfile);
delete(tempfile2);

end