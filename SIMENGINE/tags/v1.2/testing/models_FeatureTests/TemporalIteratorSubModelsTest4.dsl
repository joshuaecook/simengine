model (x1,x2) = Sub(step)

    iterator t1 with {continuous,solver=forwardeuler{dt=1}}
    iterator t2 with {continuous,solver=forwardeuler{dt=1}}
    state x1 = 0 with {iter=t1}
    state x2 = 0 with {iter=t2}
    equation x1' = step
    equation x2' = step*2
    
end

model (ya,yb,ya1,yb1,ya2,yb2)=TemporalIteratorSubModelsTest4

    iterator t1 with {continuous,solver=forwardeuler{dt=1}}
    iterator t2 with {continuous,solver=forwardeuler{dt=1}}

    submodel Sub s1 with {step=1}
    submodel Sub s2 with {step=2}
    
    output ya = (s1.x1, s2.x1)
    output yb = (s1.x2, s2.x2)
    output ya1[t1] = (s1.x1, s1.x2)
    output yb1[t2] = (s2.x1, s2.x2)
    output ya2[t1] = (s1.x1, s2.x2)
    output yb2[t2] = (s1.x2, s2.x1)

end
