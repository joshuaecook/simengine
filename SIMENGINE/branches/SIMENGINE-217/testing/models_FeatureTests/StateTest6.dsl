model (x) = StateTest6(init)

    state x = init

    equation x' = 1
    
    solver=forwardeuler{dt=1}
end
