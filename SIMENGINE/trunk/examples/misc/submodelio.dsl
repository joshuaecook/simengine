model (y) = Test(x)
  output y = x
end

model (y) = Test2
iterator i = 1..10
submodel Test t[i] with {x=i*2+1}
output y = t.y
end

