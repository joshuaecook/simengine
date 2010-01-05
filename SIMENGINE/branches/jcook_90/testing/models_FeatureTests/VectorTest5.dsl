model (out1, out2) = VectorTest5
    iterator i = 1..10

    state y[i] = 0
    equation y'[i] = y[i]

    equation out1 = y

    equation out2[i] = y[i]

    solver=forwardeuler{dt=1}
end
