//functions
function f(x) = "hi " + x

print (f(3))
print (f("chris"))

function f(x) = "hello " + x

print (f(3))
print (f("chris"))

overload function f(x:Number) = "Borg member 7 of " + x

print (f(3))
print (f("chris"))

overload function f(x:Vector of _) = "A VECTOR!!!"

overload function f(x:Vector of String) = "A VECTOR OF STRING"

print (f(3))
print (f("chris"))
print (f[1,2,3])
print (f["hi"])

overload function f(x:Number) = "Borg member 2 of " + x


print (f(3))
print (f("chris"))
print (f[1,2,3])
print (f["hi"])


//tasks
task t(x)
  x
end

print (t(3))
print (t(true))

overload task t(x:String)
  foreach i in 1 .. 10 do
    print x
  end
end

print (t(3))
print (t(true))
t("hi there")
  
task t(x)
  print (x + ":)")
end

t(3)
t(true)
t("hi there")
