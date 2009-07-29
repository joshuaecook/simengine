# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Thu Oct 16 09:50:29 -0400 2008
# ========================================================================

# Demonstrate some arbitrary math using iDynamo (the semi-colon prevents printing on the screen)
$t = calc(0:1/8000:0.1);
$y1 = calc(2*sin(2*pi*50*$t));
$y2 = calc(sin(2*pi*60*$t));
$y3 = calc(3*sin(2*pi*70*$t));

# Plot each of the sine waves
plot($t:$y1:$y2:$y3, title = 3 Sine Waves (fs = 8000 Hz), xlabel = Time (sec))

# Compute the product
$y_mult = calc($y1 .* $y2 .* $y3);
plot($t:$y_mult, title = Product of 3 Sine Waves, xlabel = Time (sec))
