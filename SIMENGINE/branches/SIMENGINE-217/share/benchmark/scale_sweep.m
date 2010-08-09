function [stats] = scale_sweep (DSL, N, varargin)
% Use this script to determine the per-iteration cost for a
% simulation, relative to the number of parallel instances.
%
% Specify the range of instances by log2 in N. When given a scalar
% N, the number of instances will range 1:(2^N). With a vector N,
% the elements of N are the range
%
% Additional parameters are passed to SIMEX.
%
% Returns a structure of runtime statistics including mean, minimum,
% maximum, and standard deviation, and also the complete record of
% individual runtimes.
%
% Examples:
%
% Run from 1 to 256 (2^8) instances of FN with the OPENMP backend
% >> stats = scale_sweep(fullfile(simexamplepath,'FN/fn.dsl'), 8, '-parallelcpu');
%
% Run from 256 (2^8) to 65536 (2^16) instances with the GPU backend
% >> stats = scale_sweep(fullfile(simexamplepath,'FN/fn.dsl'), 8:16, '-gpu');
%
% Note that running the GPU backend without specifying the lower
% end of the range N will show poor performance for small numbers
% of instances. No fewer than 32 instances is recommended for the GPU.
    
    constIters = 24;
    dt = 0.1;

    if min(size(N)) < 1
        error('N may not be empty');
    end
    if min(N) < 0 || max(N) > constIters
        error('N must fall within the range 0:%d', constIters);
    end
    if isscalar(N)
        N = 0:N;
    end
    
    tavg = zeros(1, length(N));
    tmin = zeros(1, length(N));
    tmax = zeros(1, length(N));
    tstd = zeros(1, length(N));
    elapsed = zeros(length(N), 10);

    % Call once with zero time to precompile.
    simex(DSL, [0 0], varargin{:}, ...
          '-startupmessage=false', '-instances', 2^max(N));

    for i=N
        x = 2^(constIters-i);
        y = 2^i;
        
        disp(sprintf('%d iterations x %d instances', x, y));

        for j=1:10
            tic; 
            simex(DSL, x * dt, varargin{:}, ...
                  '-startupmessage=false', '-instances', y);
            elapsed(1+i,j) = toc;
            disp(sprintf('%d of 10 took %fs', j, elapsed(1+i,j)));
        end
        tmin(1+i) = min(elapsed(1+i,:));
        tmax(1+i) = max(elapsed(1+i,:));
        tavg(1+i) = mean(elapsed(1+i,:));
        tstd(1+i) = std(elapsed(1+i,:));
        disp(sprintf('%fs average %f std %fs shortest %fs longest', ...
                     tavg(1+i), tstd(1+i), tmin(1+i), tmax(1+i)));
    end
    
    stats = struct('avg',tavg,'min',tmin,'max',tmax,'std',tstd,'elapsed',elapsed);
end
