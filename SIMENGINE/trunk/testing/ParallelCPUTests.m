

function s = ParallelCPUTests(varargin)

s = Suite('Parallel CPU Tests');
s.add(DuplicateStatesTarget('-cpu'));
s.add(DuplicateStatesTarget('-parallelcpu'));
s.add(Test('split_fn submodel cpu', @()(DuplicateStates(['models_FeatureTests/split_fn.dsl'], 10, '-double', '-cpu', 2))));
s.add(Test('MRG parallel test', @RunMRGSerialvsParallel));
end


function e = DuplicateStates(model, runtime, precision, target, number)
    m = simex(model);
    states = zeros(number, length(m.default_states));
    for i=1:number
        states(i,:) = m.default_states;
    end
    o = simex(model, runtime, states, precision, target, '-quiet');
    e = all_equiv(o);
end

function s = DuplicateStatesTarget(target)

s = Suite(['Duplicate Default States ' target]);

solvers = {'forwardeuler', 'rk4', 'ode23', 'ode45', 'cvode', ...
           'cvode_stiff', 'cvode_nonstiff', 'cvode_diag', 'cvode_tridiag'};
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
    Istim = [0 100 200];
    % precompile
    simex(model);
    % Run one input at a time and concatenate the results
    for i = 1:length(Istim)
        inputs.Istim = Istim(i);
        oserial1(i) = simex(model, runtime, inputs, '-quiet', '-dontrecompile');
    end
    inputs.Istim = Istim;
    % Run all the inputs serially in a single simex invocation
    oserial2 = simex(model, runtime, inputs, '-quiet', '-cpu');
    e = equiv(oserial1, oserial2);
    if not(e)
        return;
    end

    % Run all the inputs in parallel
    oparallel = simex(model, runtime, inputs, '-quiet', '-parallelcpu');
    
    e = equiv(oserial1, oparallel);
end