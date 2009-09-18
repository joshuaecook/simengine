model (y) = ConstantTest1

    constant c = 1

    state y = 0
    equation y' = c

    solver=forwardeuler{dt=1}
end
