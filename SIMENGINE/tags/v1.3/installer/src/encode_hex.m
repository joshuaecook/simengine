function h = encode_hex(filename)

fid = fopen(filename,'r');
if fid == -1
    error('Simatra:encode_hex', 'Can not open file ''%s'' for read', filename);
end
data = fread(fid);

step = 80;
len = length(data);
cell_len = ceil(len/step);
c = cell(cell_len, 1);
j = 1;
%disp('Breaking up the data ...')
for i=1:step:len
    if i + step > len
        c{j} = data(i:end);
    else
        c{j} = data(i:(i+step-1));
    end
    j = j + 1;
end

%disp('Converting data to hex ...')
h = cell(cell_len, 1);
for j=1:cell_len
    hex = dec2hex(c{j})';
    h{j} = reshape(hex, 1, numel(hex));
end
%hex = dec2hex(data)';
%str = reshape(hex, 1, prod(size(hex)));


end