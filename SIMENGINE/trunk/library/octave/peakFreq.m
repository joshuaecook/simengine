% peakFreq.m - returns the peak frequency value based on the fft
% file descriptor
function y = peakFreq(time, trace)

%disp('Calculating the peak frequency value ...');
x = trace-mean(trace); % remove DC offset

fs = 1/(time(3)-time(2)); % calculate fs based on the first two
                          % points in time
[f,y_fft] = quickfft(x,fs); 
peak_freq = f(find(y_fft == max(y_fft))); % find the maximum value
                                          % and return the frequency
%str = sprintf(' --> peak = %g', peak_freq);
%disp(str);
%fprintf(fd, '%g', peak_freq); % write the value to the results file

y = peak_freq;

endfunction
