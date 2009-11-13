model (y)=Fcn(x)
    equation z = x^2

    output y = z
end

model (y)=AlgebraicSubModelTest1

    state x1 = 0
    equation x1' = 1

    submodel Fcn f with {x=x1}

    output y = (x1, f.y)
    t {solver=forwardeuler{dt=1}}
end
