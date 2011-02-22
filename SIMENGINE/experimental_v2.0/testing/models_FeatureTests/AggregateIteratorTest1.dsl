model (y1, y2) = AggregateIteratorTest1

    iterator n1 with {discrete, sample_period=1}
    iterator n2 with {discrete, sample_period=1}

    state x1 = 0 with {iter=n1}
    state x2 = 10 with {iter=n2}

    equation x1[n1+1] = x1[n1] + 1
    equation x2[n2+1] = x2[n2] + 1

    output y1[n1] = (x1, x2)
    output y2[n2] = (x1, x2)

end
