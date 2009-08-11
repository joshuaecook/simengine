#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <dlfcn.h>

#define SIMENGINE_MATLAB_CLIENT
#include "simengine.h"



/* Loads the given named dynamic library file.
 * Returns an opaque handle to the library.
 */
void *load_simengine(const char *name)
    {
    void *simengine;

    if (!(simengine = dlopen(name, RTLD_NOW)))
	{
	ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlopen() failed to load %s", name);
	}

    return simengine;
    }

/* Retrieves function pointers to the simengine API calls. */
simengine_api *init_simengine(void *simengine)
    {
    simengine_api *api;
    api = NMALLOC(1, simengine_api);

    api->getinterface = dlsym(simengine, "simengine_getinterface");
    api->runmodel = dlsym(simengine, "simengine_runmodel");
    api->driver = simengine;

    return api;
    }

/* Releases a library handle. The given handle and associated api may no longer be used. */
void release_simengine(simengine_api *api)
    {
    dlclose(api->driver);
    FREE(api);
    }


/* Constructs a MATLAB struct comprising the model interface.
 * Includes names and default values for inputs and states.
 * The struct is assigned to the 'interface' pointer.
 */
void mexSimengineInterface(simengine_interface *iface, mxArray **interface)
    {
    const char *field_names[] = {"version",
				 "num_inputs", "num_states", "num_outputs",
				 "input_names", "state_names", "output_names",
				 "default_inputs", "default_states", "metadata"};
    const char *meta_names[] = {"hashcode", "num_models", "solver", "precision"};

    mxArray *version;
    mxArray *input_names, *state_names, *output_names;
    mxArray *default_inputs, *default_states;
    mxArray *metadata;
    mxArray *hashcode;
    void *data;
    unsigned int i;

    // Constructs the payload.
    input_names = mxCreateCellMatrix(1, iface->num_inputs);
    state_names = mxCreateCellMatrix(1, iface->num_states);
    output_names = mxCreateCellMatrix(1, iface->num_outputs);

    for (i=0; i<iface->num_inputs || i<iface->num_states || i<iface->num_outputs; ++i)
	{
	if (i<iface->num_inputs)
	    { mxSetCell(input_names, i, mxCreateString(iface->input_names[i])); }
	if (i<iface->num_states)
	    { mxSetCell(state_names, i, mxCreateString(iface->state_names[i])); }
	if (i<iface->num_outputs)
	    { mxSetCell(output_names, i, mxCreateString(iface->output_names[i])); }
	}

    default_inputs = mxCreateDoubleMatrix(1, iface->num_inputs, mxREAL);
    data = mxGetPr(default_inputs);
    memcpy(data, iface->default_inputs, iface->num_inputs * sizeof(double));

    default_states = mxCreateDoubleMatrix(1, iface->num_states, mxREAL);
    data = mxGetPr(default_states);
    memcpy(data, iface->default_states, iface->num_states * sizeof(double));
    
    // Creates and initializes the return structure.
    *interface = mxCreateStructMatrix(1, 1, 10, field_names);

    mxSetField(*interface, 0, "num_inputs", mxCreateDoubleScalar((double)iface->num_inputs));
    mxSetField(*interface, 0, "num_states", mxCreateDoubleScalar((double)iface->num_states));
    mxSetField(*interface, 0, "num_outputs", mxCreateDoubleScalar((double)iface->num_outputs));

    mxSetField(*interface, 0, "input_names", input_names);
    mxSetField(*interface, 0, "state_names", state_names);
    mxSetField(*interface, 0, "output_names", output_names);

    mxSetField(*interface, 0, "default_inputs", default_inputs);
    mxSetField(*interface, 0, "default_states", default_states);

    // Constructs the metadata
    version = mxCreateNumericMatrix(1, 1, mxUINT32_CLASS, mxREAL);
    data = mxGetPr(hashcode);
    memcpy(data, &iface->version, sizeof(unsigned long));
    
    hashcode = mxCreateNumericMatrix(1, 1, mxUINT64_CLASS, mxREAL);
    data = mxGetPr(hashcode);
    memcpy(data, &iface->metadata->hashcode, sizeof(unsigned long long));
    
    // Creates and initializes the metadata structure
    metadata = mxCreateStructMatrix(1, 1, 4, meta_names);

    mxSetField(*interface, 0, "version", version);
    mxSetField(*interface, 0, "metadata", metadata);
    mxSetField(metadata, 0, "hashcode", hashcode);
    mxSetField(metadata, 0, "num_models", mxCreateDoubleScalar((double)iface->metadata->num_models));
    mxSetField(metadata, 0, "solver", mxCreateString(iface->metadata->solver));
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
 *     Y = SIMEX_HELPER(DLL, TIME, INPUTS, Y0)
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

    if (2 > nrhs || 4 < nrhs)
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
	
	void *simengine = load_simengine(name);
	simengine_api *api = init_simengine(simengine);

	mexSimengineInterface(api->getinterface(), plhs);

	release_simengine(api);
	}
    else
	{
	simengine_interface *iface;
	simengine_result *result;
	const mxArray *userInputs = 0, *userStates = 0;
	double *data;
	double startTime = 0, stopTime = 0;
	unsigned int ninputs, nstates;

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

		data = mxGetData(prhs[1]);
		startTime = data[0];
		stopTime = data[1];
		break;

	    }

	ninputs = userInputs ? mxGetM(userInputs) : 0;
	nstates = userStates ? mxGetM(userStates) : 0;

	void *simengine = load_simengine(name);
	simengine_api *api = init_simengine(simengine);
	simengine_alloc allocator = { MALLOC, REALLOC, FREE };
 
	iface = api->getinterface();

	if (userInputs && mxGetN(userInputs) != iface->num_inputs)
	    {
	    release_simengine(simengine);
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"INPUTS should have %d columns.", iface->num_inputs);
	    }
	if (userStates && mxGetN(userStates) != iface->num_states)
	    {
	    release_simengine(simengine);
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Y0 should have %d columns.", iface->num_states);
	    }

	result = api->runmodel(startTime, stopTime, ninputs, mxGetData(userInputs), mxGetData(userStates), &allocator);

	release_simengine(api);
	}
    }
