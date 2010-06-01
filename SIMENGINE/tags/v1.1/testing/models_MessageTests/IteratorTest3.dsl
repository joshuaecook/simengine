//settings.debug.logdof.setValue(true)
model (x) = IteratorTest3()

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}

    state x = 0
    equation x' = 1
    equation y = x[t1]

    output x[t1]

end
