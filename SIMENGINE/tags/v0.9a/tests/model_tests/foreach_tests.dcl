# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Mon Dec 29 11:14:42 -0500 2008
# ========================================================================

$list = calc(0:0.1:4.5)
foreach l in ($list)
$l
end

foreach int in (1,2,3,  5, 6)
  calc($int*$int)
end

foreach r in (0..10)
  calc(10-$r)
end

foreach i in (  hello   , world   )
  $i
end

$count = 0
foreach i in (0..2)
  foreach j in (0..3)
    calc([$i $j])
    $count = calc($count + 1);
  end
end
$count
