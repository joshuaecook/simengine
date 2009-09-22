# STG model dcl file

# Temporarily disable variables
# $settings.logging.logprecision = true
# $settings.optimization.conservativeprecision = false
# Adjust the lookup table settings
$settings.optimization.lutdepth = 2048
$settings.optimization.lutmaxwidth = 36

# Run five versions of the same model
$s1 = compile(stg.dsl, float)
$s1.enable_output(Vm)
$s1.runfor(100)
$s1.finish()
$s2 = compile(stg.dsl, qc)
$s2.enable_output(Vm)
$s2.runfor(100)
$s2.finish()
$s3 = compile(stg.dsl, qcs)
$s3.enable_output(Vm)
$s3.runfor(100)
$s3.finish()
$s4 = compile(stg.dsl, noluts)
$s4.enable_output(Vm)
$s4.runfor(100)
$s4.finish()
$s5 = compile(stg.dsl, luts)
$s5.enable_output(Vm)
$s5.runfor(100)
$s5.finish()

# Analyze the states before and after
analyze($s1, states)
analyze($s5, states)

# Show a plot
plot(s1.traces.Vm, s2.traces.Vm, s3.traces.Vm, s4.traces.Vm, s5.traces.Vm)
