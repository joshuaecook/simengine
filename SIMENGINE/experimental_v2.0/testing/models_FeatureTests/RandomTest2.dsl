model (x, r1, r2, r3) = RandomTest2
    
    state x = 0
    equation x' = 1

    random r1 with {normal, mean=0, stddev=1}
    random r2 with {normal, mean=5, stddev=10}
    random r3 with {normal, mean=-5, stddev=5}

    solver = forwardeuler{dt=1}

end
