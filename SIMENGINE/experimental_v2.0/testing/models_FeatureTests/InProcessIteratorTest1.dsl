model (y_t1, y_t2) = InProcessIteratorTest1()

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}

    state x1 = 0 with {iter=t1}
    state x2 = 0 with {iter=t2}

    equation x1' = 1
    equation x2' = 1

    output y_t1[t1] = (x1, x1[t1[-1]], x2, x2[t2[-1]])
    output y_t2[t2] = (x1, x1[t1[-1]], x2, x2[t2[-1]])

end
