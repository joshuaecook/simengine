import sys
from os import path, environ
from subprocess import call, Popen, PIPE, STDOUT
from inspect import getfile, currentframe
from struct import calcsize
from re import search
from numpy import array, ndarray, zeros, ones

from simex_helper import simex_helper

def simex(model, time=None, inputs=None, y0=None, precision='double', target='', debug=False, profile=False, emulate=False, dontrecompile=False, verbose=False):
    dslPath, dslName, modelFile, opts = get_simex_opts(model, time=time, inputs=inputs, y0=y0, precision=precision, target=target, debug=debug, profile=profile, emulate=emulate, dontrecompile=dontrecompile, verbose=verbose)

    dll = invoke_compiler(dslPath, dslName, modelFile, **opts)
    iface = simex_helper(dll)

    if not time:
        return iface
    else:
        models = 0
        if not (inputs or y0):
            raise ArgumentError

        inputs = vet_user_inputs(iface, opts['inputs'])
        y0 = vet_user_states(iface, opts['y0'])
            
        output, y1, t1 = simex_helper(dll, (opts['startTime'], opts['stopTime']), inputs, y0)

        return (output, y1, t1)

def vet_user_inputs(iface, inputs):
    models = 0
    for key in iface['input_names']:
        if key not in inputs:
            # TODO check for NaN values in default inputs
            continue
        field = array(inputs[key])
        dims = field.shape
        
        if dims: # non-scalar
            if models <= 1:
                models = max(dims)
            elif models != max(dims):
                raise ArgumentError
        elif 0 == models:
            models = 1

    userInputs = zeros([models, iface['num_inputs']])
    for ix, key in enumerate(iface['input_names']):
        if key not in inputs:
            userInputs[:,ix] = iface['default_inputs'][key] * ones([models,1])
            continue

        field = array(inputs[key])
        dims = field.shape
        if dims:
            userInputs[:,ix] = field
        else:
            userInputs[:,ix] = field * ones([models,1])
        
    userInputs

def vet_user_states(iface, states):
    states = array(states)
    dims = states.shape
    if dims[0] != iface['num_states']:
        raise ArgumentError
    return states

def invoke_compiler(dslPath, dslName, modelFile, **opts):
    # environ['SIMENGINE'] = opts['simengine']
    simEngine = path.join(environ['SIMENGINE'], 'bin', 'simEngine')
    
    if opts['recompile']:
        status = simEngine_wrapper(simEngine, modelFile, dslName, **opts)

    # Size of a pointer, i.e. 8 on 64-bit systems
    ptrsize = calcsize('P')
    if 8 == ptrsize:
        arch = 'x86_64'
    else:
        arch = 'i386'

    make = '''make remake -f %s SIMENGINEDIR=%s MODEL=%s TARGET=%s ARCH=%s SIMENGINE_STORAGE=%s NUM_MODELS=%d''' % (
        path.join(environ['SIMENGINE'], 'share/simEngine/Makefile'),
        environ['SIMENGINE'],
        modelFile,
        opts['target'],
        arch,
        opts['precision'],
        opts['models'])

    make = ['make', 'remake',
            '-f', path.join(environ['SIMENGINE'], 'share/simEngine/Makefile'),
            'SIMENGINEDIR=' + environ['SIMENGINE'],
            'MODEL=' + modelFile,
            'TARGET=' + opts['target'],
            'ARCH=' + arch,
            'SIMENGINE_STORAGE=' + opts['precision'],
            'NUM_MODELS=%d' % opts['models']]


    if opts['emulate']:
        make.append('EMULATE=1')
    if opts['profile']:
        make.append('PROFILE=1')
    if opts['debug']:
        make.append('DEBUG=1')

    make.append('&> simex_make.log')

    status = call(make)

    if 0 != status:
        raise RuntimeError, "Make returned status %d." % (status,)

    if 'darwin' == sys.platform:
        dll = 'libsimengine.dylib'
    else:
        dll = 'libsimengine.so'

    return path.join(path.curdir, dll)    

def get_simex_opts(model, **opts):
    opts['recompile'] = not opts['dontrecompile']

    # Time limits may be specified as a scalar value, or a pair.
    # We can also accept a list of length 1.
    if hasattr(opts['time'],'__len__'):
       if 1 == len(opts): 
           opts['startTime'], opts['stopTime'] = 0.0, float(opts['time'][0])
       elif 2 == len(opts):
           opts['startTime'], opts['stopTime'] = [float(x) for x in opts['time']]
       else:
           raise RuntimeError, "Time must be a scalar value or a pair of values."
    elif hasattr(opts['time'],'__float__'):
        opts['startTime'], opts['stopTime'] = 0.0, float(opts['time'])
    
    if not opts['inputs']: 
        opts['inputs'] = dict()

    if opts['y0']: 
        opts['y0'] = array(opts['y0'])
    else: 
        opts['y0'] = ndarray(0)

    # inspect.getfile() is supposed to be more robust than __file__
    # opts['simengine'] = path.realpath(path.split(getfile(currentframe()))[0])

    # Locates the DSL source file.
    modelFile = path.realpath(model)
    if not path.exists(modelFile):
        raise IOError, "File '%s' does not exist." % (modelFile,)

    dslPath, dslName = path.split(path.splitext(modelFile)[0])
    if not dslPath: 
        dslPath = path.realpath(path.curdir)

    # Computes the number of parallel models.
    models = max(1, opts['y0'].shape[0])
    for value in opts['inputs'].itervalues():
        models = max(models, len(value))
    opts['models'] = models
    
    if not opts['target']:
        opts['target'] = 'CPU'
        if 1 < opts['models']:
            opts['target'] = 'OPENMP'

    return (dslPath, dslName, modelFile, opts)

def simEngine_wrapper(simEngine, modelFile, dslName, **opts):
    status = 1
    
    command = [simEngine, '-batch']

    # Communicates with a simEngine subprocess.
    proc = Popen(command,stdin=PIPE,stdout=PIPE,stderr=STDOUT)

    # Imports and compiles the DSL model.
    proc.stdin.write('''import "%s"\n''' % modelFile)
    proc.stdin.write('''print(compile(%s))\n''' % dslName)
    proc.stdin.close()

    # Checks for errors in the output.
    # TODO more comprehensive status reporting.
    for line in proc.stdout.readlines():
        if search("Compilation Finished Successfully", line):
            status = 0
    proc.stdout.close()
    proc.wait()

    return status
