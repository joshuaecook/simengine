// Creating a random walk for a state
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
model (x_uniform, r_uniform, x_normal, r_normal) = random_walk()

    state x_uniform = 1
    state x_normal = 1
    random r_uniform with {uniform, high=10, low=-9} 
    random r_normal with {normal, mean=0.5, stddev=9}
    equation x_uniform' = (r_uniform-0.5)
    equation x_normal' = (r_normal-0.5)
    solver=forwardeuler{dt=1}

end
