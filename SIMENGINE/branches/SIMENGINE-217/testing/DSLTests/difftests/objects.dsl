class test1 
  quantity a
  constructor (x, y)
    a := [x, y]
  end

  function f(x,y) := x + y

  task t(z) returns z:Number
    z
  end

end

interface interface1
  constructor (a)

  function f (x: Number, y: Number)

  task t(z: Vector)
end

class test2 extends test1
  quantity c

  constructor (a)
    parent(a, 1)
    
    c := 3
  end

  overload task t(z:Vector of Number)
    a := z
  end
end

class test3 extends test2 satisfies interface1
end
      
test2 satisfies interface1

class test4 extends test2
  override task t(z: Vector of Vector of Number)
    z
  end
end

namespace namespace1

end
