model (x, r1, r2, r3) = RandomTest1
    
    state x = 0
    equation x' = 1

    random r1 with {uniform, low=-10, high=0}
    random r2 with {uniform, low=0, high=10}
    random r3 with {uniform, low=-5, high=5}

    solver = forwardeuler{dt=1}

end
