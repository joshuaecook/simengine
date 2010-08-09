model (x, r1, r2) = RandomTest3
    
    state x = 0
    equation x' = 1

    random r1 with {normal, mean=0, stddev=1}
    random r2 with {uniform, low=-10, high=0}

    solver = forwardeuler{dt=1}

end
