//settings.debug.logdof.setValue(true)
model (x) = IteratorTest4()

    state x = 0 
    equation x' = 1 + x[t[1]]

    output x

end
