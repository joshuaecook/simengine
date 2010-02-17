model (y) = ConstantTest2
    constant c = 1
    // it is not permissible to reuse constant names within the same scope
    constant c = 2

    state y = 0
    equation y' = c

    solver=forwardeuler{dt=1}
end
