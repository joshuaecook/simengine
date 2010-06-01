model (y) = ConstantTest4

    constant c = 1
    state c = 2

    state y = 0
    equation y' = c

    solver=forwardeuler{dt=1}
end
