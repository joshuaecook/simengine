//settings.debug.logdof.setValue(true)
model (x) = IteratorTest6()

    state x = 0 with {iter=n}
    equation x[n+2] = x[n-1]

    output x

end
