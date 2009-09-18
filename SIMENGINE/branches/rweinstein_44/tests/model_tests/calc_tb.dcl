# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Thu Oct 16 09:50:29 -0400 2008
# ========================================================================

# Demonstrate some arbitrary math using iDynamo (the semi-colon prevents printing on the screen)
$last_t = 0.1
$t = calc(0:1/8000:$last_t);
$y1 = calc(2*sin(2*pi*50*$t));
$y2 = calc(sin(2*pi*60*$t));
$y3 = calc(3*sin(2*pi*70*$t));

# Plot each of the sine waves
#plot($t:$y1:$y2:$y3, title = 3 Sine Waves (fs = 8000 Hz), xlabel = Time (sec))

# Compute the product
$y_mult = calc($y1 .* $y2 .* $y3);
#plot($t:$y_mult, title = Product of 3 Sine Waves, xlabel = Time (sec))

# Verify that the lengths are all correct
# Basic size tests (this verifies that the octave commands worked properly)
info(First check vector sizes and time)
assert(all(length($t)*[1 1 1 1] == [length($y1) length($y2) length($y3) length($y_mult)]))
assert(first($t) == 0 && last($t) == $last_t)
assert(all(withinRange(diff($t), 0.0001, 0.00015)))

info(Pull out the frequencies found, make sure they are all correct)
$f1 = calc(peakFreq($t, $y1))
$f2 = calc(peakFreq($t, $y2))
$f3 = calc(peakFreq($t, $y3))
assert(all(withinPercent(diff([$f1 $f2 $f3]), 10, 5)))

calc([f, y_fft] = quickfft($y_mult, 8000));
$f = calc(f);
$y_fft = calc(y_fft);
#plot($f:$y_fft, title = FFT of 3 Sine Waves, xlabel = Frequency (Hz), ylabel = Intensity)

assertion_report