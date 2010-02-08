function str = encode_hex(filename)

fid = fopen(filename,'r');
data = fread(fid);
hex = dec2hex(data)';
str = reshape(hex, 1, prod(size(hex)));

end