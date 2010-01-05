// Creating a random walk for a state
// Copyright 2009 Simatra Modeling Technologies, L.L.C.
model (x, r) = random_walk()

    state x = 1
    random r with {uniform, high=10, low=-9} 
    equation x' = (r-0.5)
    solver=forwardeuler{dt=1}

end
