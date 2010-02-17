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

model (y1, y2, y3) = Middle2 (step)
    submodel Middle1 n1 with {step = step}
    submodel Middle1 n2 with {step = 2 * step}
    submodel Bottom b1 with {step = 3 * step}

    output y1 = n1.y1
    output y2 = n2.y2
    output y3 = b1.y
end


model (y) = HierarchySubModelTest3 (step)
    input step with {default = 1}


    submodel Middle2 m2 with {step = 2 * step}
    submodel Middle1 m1 with {step = step}
    submodel Bottom b1 with {step = 3 * step}

    output y = (b1.y, m2.y2, m1.y1)
    t {solver=forwardeuler{dt=1}}
end
