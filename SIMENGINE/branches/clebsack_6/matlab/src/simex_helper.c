#define SIMENGINE_MATLAB_CLIENT
// Need to define a storage class even though this code will not be
// manipulating device storage.
#define SIMENGINE_STORAGE_double
#define TARGET_CPU
#include <simengine.h>

#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <dlfcn.h>
#include <omp.h>


/* Loads the given named dynamic library file.
 * Returns an opaque handle to the library.
 */
void *load_simengine(const char *name)
    {
    void *simengine;

    if (!(simengine = dlopen(name, RTLD_NOW)))
	{
	ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlopen() failed to load %s: %s", name, dlerror());
	}

    return simengine;
    }

/* Retrieves function pointers to the simengine API calls. */
simengine_api *init_simengine(void *simengine)
    {
    simengine_api *api;
    char *msg;
    api = NMALLOC(1, simengine_api);

    api->getinterface = (simengine_getinterface_f)dlsym(simengine, "simengine_getinterface");
    if (0 != (msg = dlerror()))
	{ 
	ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load getinterface: %s", msg); 
	}
    api->runmodel = (simengine_runmodel_f)dlsym(simengine, "simengine_runmodel");
    if (0 != (msg = dlerror()))
	{ 
	ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load runmodel: %s", msg); 
	}

    api->driver = simengine;

    return api;
    }

/* Releases a library handle. The given handle and associated api may no longer be used. */
void release_simengine(simengine_api *api)
    {
    dlclose(api->driver);
    FREE(api);
    }


void mexSimengineResult(const simengine_interface *iface, int noutput, mxArray **output, unsigned int models, simengine_result *result)
    {
    unsigned int modelid, outputid, outputs = iface->num_outputs;
    simengine_output *outp = result->outputs;
    mxArray *outmat;

    // Creates the return structure.
    output[0] = mxCreateStructMatrix(models, 1, outputs, iface->output_names);
    
    // Initializes the fields for each named output.
    for (modelid = 0; modelid < models; ++modelid)
	{
	for (outputid = 0; outputid < outputs; ++outputid)
	    {
	    outmat = mxCreateDoubleMatrix(outp->num_quantities, outp->num_samples, mxREAL);
	    mxFree(mxGetPr(outmat));
	    mxSetPr(outmat, outp->data);

	    mxDestroyArray(mxGetField(*output, modelid, iface->output_names[outputid]));
	    mxSetField(*output, modelid, iface->output_names[outputid], outmat);

	    ++outp;
	    }
	}

    if (1 < noutput)
	{ 
	outmat = mxCreateDoubleMatrix(iface->num_states, models, mxREAL);
	mxFree(mxGetPr(outmat));
	mxSetPr(outmat, result->final_states);
	output[1] = outmat;
	}
    if (2 < noutput)
	{
	outmat = mxCreateDoubleMatrix(1, models, mxREAL);
	mxFree(mxGetPr(outmat));
	mxSetPr(outmat, result->final_time);
	output[2] = outmat; 
	}
    }

/* Constructs a MATLAB struct comprising the model interface.
 * Includes names and default values for inputs and states.
 * The struct is assigned to the 'interface' pointer.
 */
void mexSimengineInterface(const simengine_interface *iface, mxArray **interface)
    {
    const char *field_names[] = {"version", "name",
				 "num_inputs", "num_states", "num_outputs",
				 "input_names", "state_names", "output_names",
				 "default_inputs", "default_states", 
				 "output_num_quantities", "metadata"};
    const char *meta_names[] = {"hashcode", "num_models", "solver", "precision"};

    mxArray *version;
    mxArray *input_names, *state_names, *output_names;
    mxArray *default_inputs, *default_states;
    mxArray *output_num_quantities;
    mxArray *metadata;
    mxArray *hashcode;
    void *data;
    unsigned int i;

    // Constructs the payload.
    default_inputs = mxCreateStructMatrix(1, 1, iface->num_inputs, iface->input_names);

    default_states = mxCreateDoubleMatrix(1, iface->num_states, mxREAL);
    data = mxGetPr(default_states);
    memcpy(data, iface->default_states, iface->num_states * sizeof(double));

    output_num_quantities = mxCreateDoubleMatrix(1, iface->num_outputs, mxREAL);

    input_names = mxCreateCellMatrix(1, iface->num_inputs);
    state_names = mxCreateCellMatrix(1, iface->num_states);
    output_names = mxCreateCellMatrix(1, iface->num_outputs);

    data = mxGetPr(output_num_quantities);
    for (i = 0; i < iface->num_inputs || i < iface->num_states || i < iface->num_outputs; ++i)
	{
	if (i < iface->num_inputs)
	    { 
	    mxSetCell(input_names, i, mxCreateString(iface->input_names[i])); 

	    mxDestroyArray(mxGetField(default_inputs, 0, iface->input_names[i]));
	    mxSetField(default_inputs, 0, iface->input_names[i], mxCreateDoubleScalar((double)iface->default_inputs[i]));
	    }
	if (i < iface->num_states)
	    { mxSetCell(state_names, i, mxCreateString(iface->state_names[i])); }
	if (i < iface->num_outputs)
	    { 
	    mxSetCell(output_names, i, mxCreateString(iface->output_names[i])); 
	    ((double *)data)[i] = iface->output_num_quantities[i];
	    }
	}

    // Creates and initializes the return structure.
    *interface = mxCreateStructMatrix(1, 1, 12, field_names);

    mxDestroyArray(mxGetField(*interface, 0, "name"));
    mxSetField(*interface, 0, "name", mxCreateString(iface->name));
    
    mxDestroyArray(mxGetField(*interface, 0, "num_inputs"));
    mxSetField(*interface, 0, "num_inputs", mxCreateDoubleScalar((double)iface->num_inputs));
    mxDestroyArray(mxGetField(*interface, 0, "num_states"));
    mxSetField(*interface, 0, "num_states", mxCreateDoubleScalar((double)iface->num_states));
    mxDestroyArray(mxGetField(*interface, 0, "num_outputs"));
    mxSetField(*interface, 0, "num_outputs", mxCreateDoubleScalar((double)iface->num_outputs));

    mxDestroyArray(mxGetField(*interface, 0, "input_names"));
    mxSetField(*interface, 0, "input_names", input_names);
    mxDestroyArray(mxGetField(*interface, 0, "state_names"));
    mxSetField(*interface, 0, "state_names", state_names);
    mxDestroyArray(mxGetField(*interface, 0, "output_names"));
    mxSetField(*interface, 0, "output_names", output_names);

    mxDestroyArray(mxGetField(*interface, 0, "default_inputs"));
    mxSetField(*interface, 0, "default_inputs", default_inputs);
    mxDestroyArray(mxGetField(*interface, 0, "default_states"));
    mxSetField(*interface, 0, "default_states", default_states);

    mxDestroyArray(mxGetField(*interface, 0, "output_num_quantities"));
    mxSetField(*interface, 0, "output_num_quantities", output_num_quantities);

    // Constructs the metadata
    version = mxCreateNumericMatrix(1, 1, mxUINT32_CLASS, mxREAL);
    data = mxGetPr(version);
    memcpy(data, &iface->version, sizeof(unsigned long));

    hashcode = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    data = mxGetPr(hashcode);
    memcpy(data, &iface->metadata->hashcode, sizeof(unsigned long long));
    
    // Creates and initializes the metadata structure
    metadata = mxCreateStructMatrix(1, 1, 4, meta_names);

    mxDestroyArray(mxGetField(*interface, 0, "version"));
    mxSetField(*interface, 0, "version", version);
    mxDestroyArray(mxGetField(*interface, 0, "metadata"));
    mxSetField(*interface, 0, "metadata", metadata);
    mxDestroyArray(mxGetField(metadata, 0, "hashcode"));
    mxSetField(metadata, 0, "hashcode", hashcode);
    mxDestroyArray(mxGetField(metadata, 0, "num_models"));
    mxSetField(metadata, 0, "num_models", mxCreateDoubleScalar((double)iface->metadata->num_models));
    mxDestroyArray(mxGetField(metadata, 0, "solver"));
    mxSetField(metadata, 0, "solver", mxCreateString(iface->metadata->solver));
    mxDestroyArray(mxGetField(metadata, 0, "precision"));
    mxSetField(metadata, 0, "precision",  mxCreateDoubleScalar((double)iface->metadata->precision));
    }

void usage(void)
    {
    PRINTF("Usage: SIMEX_HELPER(DLL, '-query')\n       SIMEX_HELPER(DLL,T,INPUTS,Y0)");
    }

/* MATLAB entry point.
 *
 * Usage:
 *     M = SIMEX_HELPER(DLL, '-query')
 *     [OUT Y1] = SIMEX_HELPER(DLL, TIME, INPUTS, Y0)
 *
 *     The first form returns a struct describing the model interface,
 *     including names and default values for inputs and states.
 *
 *     DLL is the fully-qualified filename of a dynamic library
 *     adopting the simEngine API.
 *
 *     The second form executes the simulation and returns its outputs.
 *
 *     TIME is a double vector of 2 elements. The simulation starts at
 *     T=TIME(1) and proceeds to T=TIME(2).
 *
 *     INPUTS is a double vector of user-specified inputs. The number
 *     of rows corresponds to the number of parallel models.
 *
 *     Y0 is a double vector of user-specified initial states. The
 *     number of rows corresponds to the number of parallel models.
 */
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
    {
    char name[2048];

    if (!(2 == nrhs || 4 == nrhs))
	{
	usage();
	ERROR(Simatra:SIMEX:HELPER:argumentError, 
	    "Incorrect number of arguments.");
	}

    if (mxCHAR_CLASS != mxGetClassID(prhs[0]))
	{
	usage();
	ERROR(Simatra:SIMEX:HELPER:argumentError, 
	    "DLL must be a string.");
	}

    if (0 != mxGetString(prhs[0], name, 2048))
	{
	ERROR(Simatra:SIMEX:HELPER:argumentError,
	    "DLL exceeds maximum acceptable length of %d.", 2048);
	}

    if (mxCHAR_CLASS == mxGetClassID(prhs[1]))
	{
	char flag[48];
	if (0 != mxGetString(prhs[1], flag, 48))
	    {
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Second argument exceeds maximum acceptable length of %d.", 48);
	    }
	if (0 != strncasecmp("-query", flag, 7))
	    {
	    usage();
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Unrecognized argument %s.", flag);
	    }
	if (1 != nlhs)
	    {
	    usage();
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Incorrect number of left-hand side arguments.");
	    }
	
	simengine_api *api = init_simengine(load_simengine(name));
	const simengine_interface *iface = api->getinterface();

	mexSimengineInterface(iface, plhs);

	release_simengine(api);
	}
    else
	{
	simengine_result *result;
	const mxArray *userInputs = 0, *userStates = 0;
	double *data;
	double startTime = 0, stopTime = 0;
	unsigned int models, expected;

	// TODO switch is unnecessary; this form should ONLY accept 4 rhs arguments.
	switch (nrhs)
	    {
	    case 4:
		if (!mxIsDouble(prhs[3]))
		    {
		    usage();
		    ERROR(Simatra:SIMEX:HELPER:argumentError,
			"Incorrect type of Y0 argument.");
		    }
		userStates = prhs[3];
		// Passes through

	    case 3:
		if (!mxIsDouble(prhs[2]))
		    {
		    usage();
		    ERROR(Simatra:SIMEX:HELPER:argumentError,
			"Incorrect type of INPUTS argument.");
		    }
		userInputs = prhs[2];
		// Passes through

	    case 2:
		if (!mxIsDouble(prhs[2]))
		    {
		    usage();
		    ERROR(Simatra:SIMEX:HELPER:argumentError,
			"Incorrect type of TIME argument.");
		    }
		if (2 != mxGetNumberOfElements(prhs[1]))
		    {
		    ERROR(Simatra:SIMEX:HELPER:argumentError,
			"TIME must contain 2 elements.");
		    }

		data = (double*)mxGetPr(prhs[1]);
		startTime = data[0];
		stopTime = data[1];
		break;

	    }

	if (!userStates)
	    { ERROR(Simatra:SIMEX:HELPER:argumentError, "Y0 was not specified."); }
	if (!userInputs)
	    { ERROR(Simatra:SIMEX:HELPER:argumentError, "INPUTS was not specified."); }

	models = userStates ? mxGetN(userStates) : 0;

	if (mxGetN(userInputs) != models)
	    { ERROR(Simatra:SIMEX:HELPER:argumentError, "INPUTS and Y0 must be the same length."); }
	if (1 > models)
	    { ERROR(Simatra:SIMEX:HELPER:argumentError, "No models can be run."); }

	if (3 < nlhs)
	    {
	    usage();
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Incorrect number of left-hand side arguments.");
	    }

	simengine_api *api = init_simengine(load_simengine(name));
	const simengine_interface *iface = api->getinterface();
	simengine_alloc allocator = { MALLOC, REALLOC, FREE };

	mxArray *returnStates = mxDuplicateArray(userStates);

	omp_set_num_threads(omp_get_num_procs());

	result = api->runmodel(startTime, stopTime, models, mxGetPr(userInputs), mxGetPr(returnStates), &allocator);

	switch (result->status)
	    {
	    case ERRMEM:
		release_simengine(api);
		ERROR(Simatra:SIMEX:HELPER:memoryError, "Ran out of memory during simulation.");
		break;

	    case ERRCOMP:
		release_simengine(api);
		ERROR(Simatra:SIMEX:HELPER:runtimeError, "An error occurred during simulation computation.");
		break;

	    case ERRNUMMDL:
		expected = iface->metadata->num_models;
		release_simengine(api);
		ERROR(Simatra:SIMEX:HELPER:valueError, "Expected to run %d parallel models but received %d.", expected, models);
		break;
	    }

	mexSimengineResult(iface, nlhs, plhs, models, result);

	release_simengine(api);
	}
    }
