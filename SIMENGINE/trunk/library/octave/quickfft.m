%function [f,y_fft] = quickfft(y,fs);
function [f,y_fft] = quickfft(y,fs);

if nargin < 2
    disp('Usage: function [f,y_fft] = quickfft(y,fs)');
    return;
end

n = length(y);
f = fs*(0:n/2-1)/n;
y_fft = abs(fft(y,n));
y_fft = y_fft(1:n/2);

