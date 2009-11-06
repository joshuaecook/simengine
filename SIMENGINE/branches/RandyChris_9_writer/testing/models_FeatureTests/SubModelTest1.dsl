model (x)=Sub
    state x = 0
    equation x' = 1
end

model (y)=SubModelTest1

    state x1 = 0
    equation x1' = 2

    submodel Sub s
    equation x2 = s.x

    output y = (x1, x2)
end
