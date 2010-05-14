model (y) = AggregateIteratorTest6

    random r with {uniform, low=1, high=2}
    iterator t with {continuous, solver=forwardeuler{dt=1}}

    state x1 = 0

    equation x1' = r

    output y = x1

end
