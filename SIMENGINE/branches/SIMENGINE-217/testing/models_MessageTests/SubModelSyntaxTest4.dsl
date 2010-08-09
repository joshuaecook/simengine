model (y)=S(x)
  equation y = x^2
end

model (x)=SubModelSyntaxTest4

  state x = 1
  submodel S s 
  equation s.x = sqrt(x)
  equation x' = s.y

end
