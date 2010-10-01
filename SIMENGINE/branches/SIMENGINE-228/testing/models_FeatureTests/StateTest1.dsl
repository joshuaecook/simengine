model (x,y) = StateTest1

    state x = 1
    state y = 0

    equation x' = 1
    equation y' = 2    
    
    solver=forwardeuler{dt=1}
end
