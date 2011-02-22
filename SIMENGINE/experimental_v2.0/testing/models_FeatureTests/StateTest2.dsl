model (x,y) = StateTest2

    state x = 1
    state y = 5

    equation x' = 1
    
    solver=forwardeuler{dt=1}
end
