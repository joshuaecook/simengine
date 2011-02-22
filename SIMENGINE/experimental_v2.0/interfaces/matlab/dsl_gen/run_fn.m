function run_fn

    % Create a model object
    m = create_fn;
    
    % View the model
    type(m)
    
    % Write a dsl file
    dsl = toDSL(m);
    
    % Run a simulation
    o = simex(dsl, 100);
    
    % Plot the output
    simplot(o);

end