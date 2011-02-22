model (y) = ConstantTest3

    constant c = 1
    // it is not permissible to reuse identifiers within a model definition
    equation c = 2

    state y = 0
    equation y' = c

    solver=forwardeuler{dt=1}
end
