% APPEND_STR_TO_FILE - appends a text file to a new file, so that
% it can be return as a string to the calling program.
%
% function append_str_to_file(textfile, mfile, new_file, functionname)
function append_str_to_file(textfile, mfile, new_file, functionname)

datastr = encode_hex(textfile);

orig_m = fopen(mfile, 'r');
copyfile(mfile, new_file);

fid = fopen(new_file, 'a');

[directory, filename, ext] = fileparts(textfile);
restored_file = ['restored_' filename ext];

fprintf(fid, '\n');
fprintf(fid, '%% appending bin to file\n');
fprintf(fid, 'function str = %s()\n', functionname);
fprintf(fid, 'filename = ''%s'';\n', restored_file);

if ischar(datastr)
strdata = ['strdata = ''', datastr, ''';\n'];
fprintf(fid, strdata);
elseif iscell(datastr)
    fprintf(fid, 'celldata = {...\n');
    for j=1:length(datastr)
        fprintf(fid, '   ''%s''...\n', datastr{j});
    end
    fprintf(fid, '};\n');
    fprintf(fid, 'strdata = strcat(celldata{:});\n');
end
fprintf(fid, 'str = decode_hex(strdata);\n');
fprintf(fid, 'end\n');

fclose(fid);

disp(sprintf('Completed appending "%s" to create "%s"', textfile, new_file));

end
