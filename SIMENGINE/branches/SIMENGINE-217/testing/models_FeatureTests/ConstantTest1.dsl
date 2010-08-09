model (y) = ConstantTest1
    constant c = 1
    // it it permissible to override a builtin constant within a confined scope
    constant pi = 3

    state y = 0
    equation y' = c

    solver=forwardeuler{dt=1}
end
