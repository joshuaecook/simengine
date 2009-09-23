function demo = demo_models


i = 1; % index for the demos
demo = struct();

% start with a brk model
demo(i).title = 'BRK - Spike Train';
demo(i).file = 'examples/brk.dsl';
demo(i).starttime = 0;
demo(i).stoptime = 100;
demo(i).inputs.Iext = [0 40];
demo(i).steps = 50;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

i = i + 1;
% next, go to stg
demo(i).title = 'STG - Adjust Sodium';
demo(i).file = 'examples/stg.dsl';
demo(i).starttime = 0;
demo(i).stoptime = 500;
demo(i).inputs.gNa = [0 200];
demo(i).steps = 50;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

i = i + 1;
% next, try FN
demo(i).title = 'FN - Sweep Current';
demo(i).file = 'examples/fn.dsl';
demo(i).starttime = 0;
demo(i).stoptime = 100;
demo(i).inputs.I = [0 4];
demo(i).steps = 100;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

end