//settings.debug.logdof.setValue(true)
model (x) = IteratorTest5()

    state x = 0 with {iter=n}
    equation x[n] = x[n-1]

    output x

end
