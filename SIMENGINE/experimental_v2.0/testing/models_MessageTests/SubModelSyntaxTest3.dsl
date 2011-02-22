model (y)=S(x)

equation y = x^2

end

model (x)=SubModelSyntaxTest3

state x = 0

submodel S s

equation x' = s.y


end
