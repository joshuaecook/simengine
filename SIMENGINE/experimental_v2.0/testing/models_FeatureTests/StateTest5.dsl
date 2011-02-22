model (x) = StateTest5

    constant init = 1
    state x = init

    equation x' = 1
    
    solver=forwardeuler{dt=1}
end
