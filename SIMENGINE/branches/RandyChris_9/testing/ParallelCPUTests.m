

function s = ParallelCPUTests(~)

s = Suite('Parallel CPU Tests');
s.add(DuplicateStatesTarget('-cpu'));
s.add(DuplicateStatesTarget('-parallelcpu'));
s.add(Test('split_fn submodel cpu', @()(DuplicateStates(['../examples/' ...
                    'neural/FN/split_fn.dsl'], 10, '-double', '-cpu', ...
                                                  2))));
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

