model (y)=Fcn(x)
    equation z = x^2

    output y = 3 * z
end

model (y)=AlgebraicSubModelTest2

    iterator t1 with {continuous, solver=forwardeuler{dt=1}}
    iterator t2 with {continuous, solver=forwardeuler{dt=1}}
    state x1 = 0 with {iter=t1}
    equation x1' = 1
    state x2 = 0 with {iter=t2}
    equation x2' = 1

    submodel Fcn f1 with {x=x1}
    submodel Fcn f2 with {x=x2}

    output y[t1] = (x1, f1.y, f2.y)
    //t {solver=forwardeuler{dt=1}}
end
