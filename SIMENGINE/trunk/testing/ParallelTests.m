

function s = ParallelTests(varargin)

target = varargin{1};

s = Suite(['Parallel Tests ' target]);

if strcmpi(target, '-cpu')
  s.add(DuplicateStatesTarget('-cpu'));
  s.add(DuplicateStatesTarget('-parallelcpu'));
  s.add(Test('split_fn submodel parallelcpu', @()(DuplicateStates('models_FeatureTests/split_fn.dsl', 10, '-double', '-parallelcpu', 2))));
  s.add(Test('fn_imp explicit/implicit parallelcpu', @()(DuplicateStates('models_FeatureTests/fn_imp.dsl',10, '-double', '-parallelcpu', 10))));
  s.add(Test('MRG parallel test', @RunMRGSerialvsParallel));

else
  % Parallel GPU tests
  s.add(Test('split_fn submodel gpu', @()(DuplicateStates('models_FeatureTests/split_fn.dsl', 10, '-double', '-gpu', 2))));
  s.add(DuplicateStatesTarget('-gpu'));
  s.add(Test('fn_imp explicit/implicit gpu', @()(DuplicateStates('models_FeatureTests/fn_imp.dsl',10, '-double', '-gpu', 10))));
end

end


function e = DuplicateStates(model, runtime, precision, target, number)
    m = simex(model);
    states = zeros(number, length(m.defaultStates));
    for i=1:number
        states(i,:) = m.defaultStates;
    end
    o = simex(model, runtime, '-resume', states, precision, target);
    e = all_equiv(o);
end

function s = DuplicateStatesTarget(target)

s = Suite(['Duplicate Default States ' target]);

if strcmp('-gpu', target)
solvers = {'forwardeuler', 'rk4', 'ode23', 'ode45'};
else
solvers = {'forwardeuler', 'rk4', 'ode23', 'ode45', 'cvode', ...
           'cvode_stiff', 'cvode_nonstiff', 'cvode_diag', 'cvode_tridiag'};
end
precisions = {'single', 'double'};
for i=1:length(solvers)
    solver = solvers{i};
    for j=1:length(precisions)
        precision = precisions{j};
        name = ['Parallel fn_' solver '_' precision];
        model = ['models_SolverTests/fn_' solver '.dsl'];
        time = 10;

        s.add(Test(name,@()(DuplicateStates(model, time, ['-' ...
                            precision], target, 10))));
    end
end

end

function e = RunMRGSerialvsParallel

    model = fullfile(simexamplepath, 'MRG/axon.dsl');
    runtime = 2;
    Istim = {0 100 200};
    oserial = zeros(size(Istim));
    % precompile
    simex(model);
    % Run one input at a time and concatenate the results
    for i = 1:length(Istim)
        inputs.Istim = Istim{i};
        oserial1(i) = simex(model, runtime, inputs, '-dontrecompile');
    end
    inputs.Istim = Istim;
    % Run all the inputs serially in a single simex invocation
    oserial2 = simex(model, runtime, inputs, '-cpu');
    e = equiv(oserial1, oserial2);
    if not(e)
        return;
    end

    % Run all the inputs in parallel
    oparallel = simex(model, runtime, inputs, '-parallelcpu');
    
    e = equiv(oserial1, oparallel);
end
