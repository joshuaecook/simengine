settings.ir.aggregate.setValue(true)
model (y1, y2) = AggregateIteratorTest2

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}

    state x1 = 0 with {iter=t1}
    state x2 = 10 with {iter=t2}

    equation x1' = 1
    equation x2' = 1

    output y1[t1] = (x1, x2)
    output y2[t2] = (x1, x2)

end
