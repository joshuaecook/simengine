model (two_const) = ConstantTest6
    
    constant two = 2

    state two_const = two

    solver=forwardeuler{dt=1}
end
