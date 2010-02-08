//settings.debug.logdof.setValue(true)
model (x) = IteratorTest8()

    state x[n] = 0 with {iter=n}
    equation x[n+1] = x[n] + 1

    output x

end
