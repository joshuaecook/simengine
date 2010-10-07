model (x) = sub()

    output x = (1, 2)

end

model (y) = OutputTest4()

   submodel sub s
    
   output y = s.x

end
