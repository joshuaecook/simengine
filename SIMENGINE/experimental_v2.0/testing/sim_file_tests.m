%% sim_file_tests(numtests, dslfile)
%
% Tests the performance of the simfile in terms of compilation time
% and reuse.  This is unable to separate out the time for the
% actual sim file, but when run on various sim file types (e.g. simlib
% vs. zip) it will demonstrate any noticeable performance changes.

function sim_file_tests(numtests, dslfile)

delete fn.sim;

tic;
for i = 1:numtests;
  evalc(['simex(''' dslfile ''')']);
  delete fn.sim;
end
totaltime = toc;
avg = totaltime/numtests;
disp(['Compilation time avg/total ' num2str(avg) ' / ' num2str(totaltime) ' seconds'])

evalc(['simex(''' dslfile ''')']);

tic;
for i = 1:numtests;
  evalc(['simex(''' dslfile ''')']);
end
totaltime = toc;
avg = totaltime/numtests;
disp(['Simfile reuse time avg/total ' num2str(avg) ' / ' num2str(totaltime) ' seconds'])
end