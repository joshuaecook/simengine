model (y) = Sub (x)
  state y = 0
  equation y' = x
end

model (y) = AlgebraicSubModelTest3
    submodel Sub s with {x = 4}
    equation y = s.y
end
