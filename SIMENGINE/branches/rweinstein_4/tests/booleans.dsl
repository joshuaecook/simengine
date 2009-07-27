quantity q := true;

print q;
print (not q);

print true;
print false;
print (not true);
print (not false);

//print {1 when true, 3 otherwise};
//print {3 when false, 4 otherwise};
if q then
  print "success"
else
  print "failure"
end;

if not q then
  print "failure"
else
  print "success"
end;


