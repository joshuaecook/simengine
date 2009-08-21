function append_bin_to_file(binfile, mfile, new_file, functionname)

datastr = encode_hex(binfile);

orig_m = fopen(mfile, 'r');
copyfile(mfile, new_file);

fid = fopen(new_file, 'a');

[directory, filename, ext] = fileparts(binfile);
restored_file = ['restored_' filename ext];

fprintf(fid, '\n');
fprintf(fid, '%% appending bin to file\n');
fprintf(fid, 'function filename = %s()\n', functionname);
fprintf(fid, 'filename = ''%s'';\n', restored_file);
strdata = ['strdata = ''', datastr, ''';\n'];
fprintf(fid, strdata);
fprintf(fid, 'decode_hex(strdata, filename);\n');
fprintf(fid, 'end\n');

fclose(fid);

disp(sprintf('Completed appending "%s" to create "%s"', binfile, new_file));

end