model (y)=S(x)
  equation y = x^2
end

model (x)=SubModelSyntaxTest5

  state x = 1
  submodel S s with {x=1, y=1}
  equation x' = s.y

end
