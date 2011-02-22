//settings.debug.logdof.setValue(true)
model (x) = IteratorTest7()

    state x = 0 with {iter=n}
    equation x[n+2] = x[2*n-1]

    output x

end
