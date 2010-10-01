model (x) = StateTest4(init)

    input init with {default=0}
    state x = init

    equation x' = 1
    
    solver=forwardeuler{dt=1}
end
