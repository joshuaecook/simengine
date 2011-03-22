function append_files(mfile, new_file, filelist)

copyfile(mfile, new_file);
f = tempname;

for i=1:length(filelist)
  if filelist(i).bin
    append_bin_to_file(filelist(i).file, new_file, f, ...
                       filelist(i).fcnname);
  else
    append_str_to_file(filelist(i).file, new_file, f, ...
                       filelist(i).fcnname);
  end
  copyfile(f, new_file);
end