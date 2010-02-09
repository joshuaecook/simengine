model (x) = sub()

    output x = 1 with {condition=t>5}

end

model (y) = OutputTest5()

   submodel sub s
    
   output y = s.x

end
