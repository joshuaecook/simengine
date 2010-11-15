% ML_Tests - return the list of MATLAB based tests
function s = ML_Tests

s = Suite('All MATLAB');

s.add(ReleaseMatlabExampleTests, {'examples'});
s.add(ML_ExpTests, {'vectorization', 'v2.0'});

s_cpu = Suite('All MATLAB on CPU', {'cpu'});
s.add(s_cpu);

s_cpu.add(ML_CoreFeatureTests('-cpu'), {'core'});
s_cpu.add(ML_SyntaxTests('-cpu'), {'syntax'});
s_cpu.add(ML_SubModelTests('-cpu'), {'submodels'});
s_cpu.add(VectorizedTests('-cpu'), {'vectorization', 'v2.0'});

s_gpu = Suite('All MATLAB on GPU', {'gpu'});
s.add(s_gpu);

s_gpu.add(ML_CoreFeatureTests('-gpu'), {'core'});
s_gpu.add(ML_SyntaxTests('-gpu'), {'syntax'});
s_gpu.add(ML_SubModelTests('-gpu'), {'submodels'});
s_gpu.add(VectorizedTests('-gpu'), {'vectorization', 'v2.0'});


end