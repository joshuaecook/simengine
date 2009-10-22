# iDynamo Command Log
# Copyright 2008, Simatra Modeling Technologies, L.L.C.
# Created Thu Oct 16 19:12:39 -0400 2008
# ========================================================================

# Compile the model - fast software simulation only
$s = compile(brk_model.dsl, luts)
$s.enable_output(V?)

# Run a steady state simulation - grab the resting membrane potentials
info(Steady State Protocol)
$s.set_param(Iext, 0)
$s.runfor(100)
$SteadyStateVs = calc(last($s.traces.Vs))
$SteadyStateVd = calc(last($s.traces.Vd))
assert($SteadyStateVs == -56.564)
assert($SteadyStateVd == -55.673)
plot($s, title = Steady State Protocol, xlabel = Time (ms), ylabel = Voltage (mV))

# Calculate input conductance
info(Input Conductance Protocol)
$s.reset()
$s.set_param(Iext, 0)
$s.runfor(5)
$InjectedCurrent = -10
$s.set_param(Iext, $InjectedCurrent)
$s.runfor(10)
$s.set_param(Iext, 0)
$s.runfor(5)
$VoltageDrop = calc(min($s.traces.Vs) - $SteadyStateVs)
# G = I/V (nA/mV = uS)
$Ginput = calc($InjectedCurrent/$VoltageDrop)
metric($Ginput)
assert(withinPercent($Ginput, 1.00, 3))
plot($s, title = Input Conductance Test, xlabel = Time (ms), ylabel = Voltage (mV))

# Perform a spike test
info(Spike Protocol)
$s.reset()
$s.set_param(Iext, 0)
$s.runfor(5)
$s.set_param(Iext, 40)
$s.runfor(1)
$s.set_param(Iext, 0)
$s.runfor(5)
$SpikeHeight = calc(spikeHeight($s.traces.Vs))
$SpikeWidth = calc(spikeWidth($s.traces.t, $s.traces.Vs))
assert(withinPercent($SpikeHeight, 100, 20))
assert($SpikeWidth > 0.3 && $SpikeWidth < 0.5)
plot($s, title = Spike Protocol Test, xlabel = Time (ms), ylabel = Voltage (mV))
metric($SpikeHeight)
metric($SpikeWidth)

# Now run five simulations, each time adding more current and record
# the spike count and spiking frequencies
info(F-I Protocol)
$currents = []
$spike_frequencies = []
foreach current in (5,10,15,20,25,30,35,40)
  $s.reset()
  $s.set_param(Iext, $current)
  $s.runfor(100)
  $spike_count = calc(length(getSpikes($s.traces.Vs)))
  if $spike_count > 1 then
    $currents = calc([$currents $current])
    $spike_times =  calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
    $spike_freq = calc(1000/last(diff($spike_times)))
    $spike_frequencies = calc([$spike_frequencies $spike_freq])
  end
  #plot($s, title = Voltage Trace (Iext = $current))
end

plot($currents:$spike_frequencies, title = F-I Relationship, xlabel = Current (nA), ylabel = Spiking Frequency (Hz))


assertion_report
exit

$s.reset()
$s.set_param(Iext, 5)
$s.runfor(100)
$spike_count_5 = calc(length(getSpikes($s.traces.Vs)))
$spike_times_5 = calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
$spike_freq_5 = calc(1000/last(diff($spike_times_5)))
assert($spike_count_5 == 2)
$Vs_5 = $s.traces.Vs;

$s.reset()
$s.set_param(Iext, 10)
$s.runfor(100)
$spike_count_10 = calc(length(getSpikes($s.traces.Vs)))
$spike_times_10 = calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
$spike_freq_10 = calc(1000/last(diff($spike_times_10)))
assert($spike_count_10 == 4)
$Vs_10 = $s.traces.Vs;

$s.reset()
$s.set_param(Iext, 15)
$s.runfor(100)
$spike_count_15 = calc(length(getSpikes($s.traces.Vs)))
$spike_times_15 = calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
$spike_freq_15 = calc(1000/last(diff($spike_times_15)))
assert($spike_count_15 == 5)
$Vs_15 = $s.traces.Vs;

$s.reset()
$s.set_param(Iext, 20)
$s.runfor(100)
$spike_count_20 = calc(length(getSpikes($s.traces.Vs)))
$spike_times_20 = calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
$spike_freq_20 = calc(1000/last(diff($spike_times_20)))
assert($spike_count_20 == 6)
$Vs_20 = $s.traces.Vs;

$s.reset()
$s.set_param(Iext, 25)
$s.runfor(100)
$spike_count_25 = calc(length(getSpikes($s.traces.Vs)))
$spike_times_25 = calc(getSpikeTimes($s.traces.t, $s.traces.Vs))
$spike_freq_25 = calc(1000/last(diff($spike_times_25)))
assert($spike_count_25 == 7)
$Vs_25 = $s.traces.Vs;

# Verify that the spiking frequencies are all increasing along with current
$spike_frequencies = calc([$spike_freq_5 $spike_freq_10 $spike_freq_15 $spike_freq_20 $spike_freq_25])
assert((all(diff($spike_frequencies) > 0)))

# Plot the current vs. spike frequency plot (F-I Gain)
$currents = calc([5 10 15 20 25])
plot($currents:$spike_frequencies, title = F-I Relationship, xlabel = Current (nA), ylabel = Spiking Frequency (Hz))

# Compare the different voltage trajectories per current input
plot($Vs_5, $Vs_10, $Vs_15, $Vs_20, $Vs_25, title = Voltage Trajectories at Varying Input Currents, xlabel = Current (nA), ylabel = Voltage (mV))

# Produce the assertion report
assertion_report

