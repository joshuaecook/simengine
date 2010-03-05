settings.ir.aggregate.setValue(true)
model (y1, y2) = AggregateIteratorTest3

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator n1 with {discrete, sample_period=1}

    state x1 = 0 with {iter=t1}
    state x2 = 10 with {iter=n1}

    equation x1' = 1
    equation x2[n1+1] = x2[n1] + 1

    output y1[t1] = (x1, x2)
    output y2[n1] = (x1, x2)

end
