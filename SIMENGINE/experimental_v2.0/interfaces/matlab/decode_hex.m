% decode the included hex file into a restored file
function decode_hex(str, filename)

%disp(sprintf('Calling decode_hex with file=%s',filename));

hex = reshape(str, 2, length(str)/2)';
data = hex2dec(hex);
fid = fopen(filename,'w');
fwrite(fid, data);
fclose(fid);

end
