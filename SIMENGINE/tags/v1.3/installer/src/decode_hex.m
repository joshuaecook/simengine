% decode the included hex file into a restored file
function [varargout] = decode_hex(str, varargin)

%disp(sprintf('Calling decode_hex with file=%s',filename));
hex = reshape(str, 2, length(str)/2)';
data = hex2dec(hex);
if nargin == 2 
  filename = varargin{1};  
  fid = fopen(filename,'w');
  if fid == -1
      error('Simatra:decode_hex', 'Can not open filename %s for write', filename);
  end
  fwrite(fid, data);
  fclose(fid);
elseif nargout==1
  varargout{1} = data;
end

end
