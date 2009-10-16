function demo = demo_models



i = 1; % index for the demos
demo = struct();

% start with a brk model
demo(i).title = 'BRK - Spike Train';
demo(i).file = [simexamplepath '/BRK/brk.dsl'];
demo(i).starttime = 0;
demo(i).stoptime = 100;
demo(i).inputs.Iext = [0 40];
demo(i).steps = 50;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

i = i + 1;
% next, go to stg
demo(i).title = 'PD - Adjust Sodium';
demo(i).file = [simexamplepath '/PD/pd.dsl'];
demo(i).starttime = 0;
demo(i).stoptime = 500;
demo(i).inputs.gNa = [0 200];
demo(i).steps = 50;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

% i = i + 1;
% % next, try pBc
% demo(i).title = 'pBc - preBotzinger Complex Persistant Na Sweep';
% demo(i).file = [simexamplepath '/pbc.dsl'];
% demo(i).starttime = 0;
% demo(i).stoptime = 2;
% demo(i).inputs.Iext = 8;
% demo(i).inputs.gNaP = [2.5 3];
% demo(i).steps = 100;
% demo(i).precision = 'single';
% demo(i).target = 'parallel-cpu';

i = i + 1;
% next, try FN
demo(i).title = 'FN - Sweep Current';
demo(i).file = [simexamplepath '/FN/fn.dsl'];
demo(i).starttime = 0;
demo(i).stoptime = 100;
demo(i).inputs.I = [0 4];
demo(i).steps = 100;
demo(i).precision = 'single';
demo(i).target = 'parallel-cpu';

end
