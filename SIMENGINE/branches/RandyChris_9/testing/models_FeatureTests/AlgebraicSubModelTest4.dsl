model (y)=Fcn(x)
    equation z = x^2

    output y[t] = z
end

model (y, z)=AlgebraicSubModelTest4

    state x1 = 0
    equation x1' = 1

    submodel Fcn f with {x=x1}

    output y = f.y
    output z = x1
    t {solver=forwardeuler{dt=1}}
end
