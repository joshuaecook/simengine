model (y) = AggregateIteratorTest4

    iterator t with {continuous, solver=forwardeuler{dt=1}}

    state x1 = 0
    state x2 = 0

    equation x1' = 1
    equation x2 = x1

    output y = (x1, x2)

end
