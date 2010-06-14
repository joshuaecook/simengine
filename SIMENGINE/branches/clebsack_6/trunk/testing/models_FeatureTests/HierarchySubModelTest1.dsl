model (y) = Bottom (step)
    state y = 0
    equation y' = step
end

model (y1, y2, y3) = Middle1 (step)
    submodel Bottom b1 with {step = step}
    submodel Bottom b2 with {step = 2 * step}
    submodel Bottom b3 with {step = 3 * step}

    output y1 = b1.y
    output y2 = b2.y
    output y3 = b3.y
end

model (y) = HierarchySubModelTest1 (step)
    input step with {default = 1}


    submodel Middle1 m1 with {step = step}
    submodel Bottom b1 with {step = 3 * step}

    output y = (b1.y, m1.y1)
    t {solver=forwardeuler{dt=1}}
end
