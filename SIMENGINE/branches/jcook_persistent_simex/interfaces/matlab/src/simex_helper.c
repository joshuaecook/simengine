#define SIMENGINE_MATLAB_CLIENT
// Need to define a storage class even though this code will not be
// manipulating device storage.
#define SIMENGINE_STORAGE_double
#define TARGET_CPU
#include <simengine_target.h>
#include <simengine_api.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <dlfcn.h>
#include <pthread.h>
#include <omp.h>
#include <assert.h>

static simengine_api *api = NULL;
static double *inputs = NULL;

//#define ERROR(ID, MESSAGE, ARG...) {mexPrintf("ERROR (%s): " MESSAGE "\n",  #ID, ##ARG); return; }

static unsigned char initialized = 0;
static pthread_t simengine_thread;

void thread_error (int code, const char *message)
    {
    switch (code)
	{
	case EINVAL: ERROR(Simatra:SIMEX:HELPER:ThreadError, "%s; (%d) invalid argument.", code, message);
	case ESRCH: ERROR(Simatra:SIMEX:HELPER:ThreadError, "%s; (%d) no such thread.", code, message);
	case EDEADLK: ERROR(Simatra:SIMEX:HELPER:ThreadError, "%s; (%d) deadlock.", code, message);
	}
    }

void *run_simengine_thread (void *arg)
    {
    PRINTF("I'm on a boat!\n");

    pthread_exit(NULL);
    return NULL;
    }

void release_simex_helper (void)
    {
    static unsigned char releasing = 0;

    if (!initialized) return;
    if (releasing) { ERROR(Simatra:SIMEX:HELPER:RunTimeError, "Attempted to double-release SIMEX_HELPER.\nPlease restart MATLAB before attempting to use SIMEX again."); }
    releasing = 1;

    PRINTF("releasing SIMEX_HELPER\n");

    if (simengine_thread)
	{
	int err = pthread_join(simengine_thread, NULL);
	if (err) thread_error(err, "Failed to join simengine thread");
	}

    initialized = 0;
    releasing = 0;
    }

void init_simex_helper (void)
    {
    static unsigned char initializing = 0;

    if (initialized) return;
    if (initializing) { ERROR(Simatra:SIMEX:HELPER:RunTimeError, "Attempted to double-initialize SIMEX_HELPER.\nPlease restart MATLAB before attempting to use SIMEX again."); }
    initializing = 1;

    PRINTF("initializing SIMEX_HELPER\n");
    mexAtExit(release_simex_helper);

	{
	// TODO need any attributes?
	// TODO what to pass as an argument?
	int err = pthread_create(&simengine_thread, NULL, run_simengine_thread, NULL);
	if (err) thread_error(err, "Failed to create simengine thread");
	}

    initialized = 1;
    initializing = 0;
    }



/* Loads the given named dynamic library file.
   Retrieves function pointers to the simengine API calls. */
void init_simengine(const char *name)
{
  char *msg;
  api = NMALLOC(1, simengine_api);
  mexMakeMemoryPersistent(api);

  api->driver = dlopen(name, RTLD_NOW);
  if(!api->driver)
    {
      ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlopen() failed to load %s: %s", name, dlerror());
    }

  api->getinterface = (simengine_getinterface_f)dlsym(api->driver, "simengine_getinterface");
  if (0 != (msg = dlerror()))
    { 
      ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load getinterface: %s", msg); 
    }
  api->runmodel = (simengine_runmodel_f)dlsym(api->driver, "simengine_runmodel");
  if (0 != (msg = dlerror()))
    { 
      ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load runmodel: %s", msg); 
    }
  api->evalflow = (simengine_evalflow_f)dlsym(api->driver, "simengine_evalflow");
  if (0 != (msg = dlerror()))
    { 
      ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load evalflow: %s", msg); 
    }
}

/* Releases a library handle. The given handle and associated api may no longer be used. */
void release_simengine()
{
  dlclose(api->driver);
  FREE(api);
  api = NULL;
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
    const unsigned int num_fields = 13;
    const char *field_names[] = {"version", "name",
				 "num_inputs", "num_states", "num_outputs", "num_iterators",
				 "input_names", "state_names", "output_names",
				 "default_inputs", "default_states", 
				 "output_num_quantities", "metadata"};
    const unsigned int num_meta = 4;
    const char *meta_names[] = {"hashcode", "num_models", "solvers", "precision"};

    mxArray *version;
    mxArray *input_names, *state_names, *output_names, *solvers;
    mxArray *default_inputs, *default_states;
    mxArray *output_num_quantities;
    mxArray *metadata;
    mxArray *hashcode;
    void *data;
    unsigned int i;

    // Constructs the payload.
    if (0 < iface->num_inputs)
        { 
	input_names = mxCreateCellMatrix(1, iface->num_inputs); 
	default_inputs = mxCreateStructMatrix(1, 1, iface->num_inputs, iface->input_names);
	}
    else
	{
	input_names = mxCreateCellMatrix(0, 0);
	default_inputs = mxCreateStructMatrix(0, 0, 0, NULL);
	}
    assert(input_names != NULL);
    assert(default_inputs != NULL);

    if (0 < iface->num_states)
        { 
	state_names = mxCreateCellMatrix(1, iface->num_states); 
	default_states = mxCreateDoubleMatrix(1, iface->num_states, mxREAL);
	data = mxGetPr(default_states);
	memcpy(data, iface->default_states, iface->num_states * sizeof(double));
	}
    else
	{
	state_names = mxCreateCellMatrix(0, 0);
	default_states = mxCreateDoubleMatrix(0, 0, mxREAL);
	}
    assert(state_names != NULL);
    assert(default_states != NULL);

    if (0 < iface->num_outputs)
        { 
	output_names = mxCreateCellMatrix(1, iface->num_outputs); 
	output_num_quantities = mxCreateDoubleMatrix(1, iface->num_outputs, mxREAL);
	}
    else
	{
	output_names = mxCreateCellMatrix(0, 0);
	output_num_quantities = mxCreateDoubleMatrix(0, 0, mxREAL);
	}
    assert(output_names != NULL);
    assert(output_num_quantities != NULL);

    if (0 < iface->num_iterators)
        { 
	solvers = mxCreateCellMatrix(1, iface->num_iterators); 
	}
    else
	{
	solvers = mxCreateCellMatrix(0, 0);
	}
    assert(solvers != NULL);

    data = mxGetPr(output_num_quantities);
    for (i = 0; i < iface->num_inputs || i < iface->num_states || i < iface->num_outputs || i < iface->num_iterators; ++i)
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
	if (i < iface->num_iterators)
	    { mxSetCell(solvers, i, mxCreateString(iface->metadata->solvers[i])); }
	}

    // Creates and initializes the return structure.
    *interface = mxCreateStructMatrix(1, 1, num_fields, field_names);

    mxDestroyArray(mxGetField(*interface, 0, "name"));
    mxSetField(*interface, 0, "name", mxCreateString(iface->name));
    
    mxDestroyArray(mxGetField(*interface, 0, "num_inputs"));
    mxSetField(*interface, 0, "num_inputs", mxCreateDoubleScalar((double)iface->num_inputs));

    mxDestroyArray(mxGetField(*interface, 0, "num_states"));
    mxSetField(*interface, 0, "num_states", mxCreateDoubleScalar((double)iface->num_states));

    mxDestroyArray(mxGetField(*interface, 0, "num_outputs"));
    mxSetField(*interface, 0, "num_outputs", mxCreateDoubleScalar((double)iface->num_outputs));

    mxDestroyArray(mxGetField(*interface, 0, "num_iterators"));
    mxSetField(*interface, 0, "num_iterators", mxCreateDoubleScalar((double)iface->num_iterators));

    mxDestroyArray(mxGetField(*interface, 0, "input_names"));
    mxSetField(*interface, 0, "input_names", input_names);

    mxDestroyArray(mxGetField(*interface, 0, "default_inputs"));
    mxSetField(*interface, 0, "default_inputs", default_inputs);

    mxDestroyArray(mxGetField(*interface, 0, "state_names"));
    mxSetField(*interface, 0, "state_names", state_names);

    mxDestroyArray(mxGetField(*interface, 0, "default_states"));
    mxSetField(*interface, 0, "default_states", default_states);

    mxDestroyArray(mxGetField(*interface, 0, "output_names"));
    mxSetField(*interface, 0, "output_names", output_names);

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
    metadata = mxCreateStructMatrix(1, 1, num_meta, meta_names);

    mxDestroyArray(mxGetField(*interface, 0, "version"));
    mxSetField(*interface, 0, "version", version);

    mxDestroyArray(mxGetField(*interface, 0, "metadata"));
    mxSetField(*interface, 0, "metadata", metadata);

    mxDestroyArray(mxGetField(metadata, 0, "hashcode"));
    mxSetField(metadata, 0, "hashcode", hashcode);

    mxDestroyArray(mxGetField(metadata, 0, "num_models"));
    mxSetField(metadata, 0, "num_models", mxCreateDoubleScalar((double)iface->metadata->num_models));

    mxDestroyArray(mxGetField(metadata, 0, "solvers"));
    mxSetField(metadata, 0, "solvers", solvers);

    mxDestroyArray(mxGetField(metadata, 0, "precision"));
    mxSetField(metadata, 0, "precision",  mxCreateDoubleScalar((double)iface->metadata->precision));
    }

void usage(void)
    {
      PRINTF("Usage:\tSIMEX_HELPER(DLL, '-query')\n\tSIMEX_HELPER(DLL,T,INPUTS,Y0)\n\tSIMEX_HELPER(DLL,INPUTS)\n\tSIMEX_HELPER(T,Y0)\n");
    }


/* MATLAB entry point.
 *
 * Usage:
 *     M = SIMEX_HELPER(DLL, '-query')
 *     [OUT Y1] = SIMEX_HELPER(DLL, TIME, INPUTS, Y0)
 *     SIMEX_HELPER(DLL, INPUTS)
 *     Y1 = SIMEX_HELPER(TIME, Y0)
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

    init_simex_helper();    

    // Fast entry to evalflow
    // Check for two parameters where the first is not the name of the DLL
    if(2 == nrhs && mxCHAR_CLASS != mxGetClassID(prhs[0])){
      if(api == NULL)
	{
	  ERROR(Simatra:SIMEX:HELPER:InitializationError,
		"simex_helper.mex was not initialized with a dynamic library to load.");
	}
      double t = mxGetScalar(prhs[0]);
      const mxArray *y0 = prhs[1];
      plhs[0] = mxCreateDoubleMatrix(mxGetM(y0), 1, mxREAL);
      double *y1 = mxGetPr(plhs[0]);
      int ret = api->evalflow(t, mxGetPr(y0), y1, inputs);
      if (0 != ret)
	{
	  ERROR(Simatra:SIMEX:HELPER:RunTimeError,
		"Evaluation of flows failed at time=%g", t);
	}
      return;
    }

    if (!(2 == nrhs || 4 == nrhs || 5 == nrhs))
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

    // Initialize or remove the DLL for the ODE interface to evalflow
    if (0 == nlhs && 2 == nrhs) 
      {
	if(api == NULL){
	  init_simengine(name);
	  inputs = mxGetPr(prhs[1]);
	}
	else
	  {
	    release_simengine();
	    inputs = NULL;
	  }
	return;
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
	
	init_simengine(name);
	const simengine_interface *iface = api->getinterface();

	mexSimengineInterface(iface, plhs);

	release_simengine();
	}
    
    else
      {
	simengine_result *result;
	const mxArray *userInputs = 0, *userStates = 0;
	double *data;
	double startTime = 0, stopTime = 0;
	unsigned int models, expected;

	if (3 < nlhs)
	    {
	    usage();
	    ERROR(Simatra:SIMEX:HELPER:argumentError,
		"Incorrect number of left-hand side arguments.");
	    }

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

	init_simengine(name);
	const simengine_interface *iface = api->getinterface();
	simengine_alloc allocator = { MALLOC, REALLOC, FREE };

	if (!userStates)
	    { 
	    release_simengine();
	    ERROR(Simatra:SIMEX:HELPER:argumentError, "Y0 was not specified."); 
	    }
	if (!userInputs)
	    { 
	    release_simengine();
	    ERROR(Simatra:SIMEX:HELPER:argumentError, "INPUTS was not specified."); 
	    }

	if (0 < iface->num_states)
	    {
	    models = mxGetN(userStates);

	    if (0 < iface->num_inputs && mxGetN(userInputs) != models)
		{
		release_simengine();
		ERROR(Simatra:SIMEX:HELPER:argumentError, "INPUTS and Y0 must be the same length %d.", models); 
		}
	    }
	else if (0 < iface->num_inputs)
	    {
	    models = mxGetN(userInputs);
	    }
	else
	    {
	    // With no states or inputs, only a single model may be run.
	    models = 1;
	    }
	
	if (1 > models)
	    { 
	    release_simengine();
	    ERROR(Simatra:SIMEX:HELPER:argumentError, "No models can be run."); 
	    }

        // These openmp calls are not used currently, but openmp may be used in the future to move data back to
        // Matlab.  For some reason, if omp_set_num_threads is called without querying the omp environment first
        // this code fails to compile and link properly on Mac OS X 10.5.
	int nt = omp_get_num_threads(); // this call in particular is needed
	int np = omp_get_num_procs();
	omp_set_num_threads(np);

	result = api->runmodel(startTime, stopTime, models, mxGetPr(userInputs), mxGetPr(userStates), &allocator);

	switch (result->status)
	    {
	    case ERRMEM:
		release_simengine();
		ERROR(Simatra:SIMEX:HELPER:memoryError, "Ran out of memory during simulation.");
		break;

	    case ERRCOMP:
		release_simengine();
		ERROR(Simatra:SIMEX:HELPER:runtimeError, "An error occurred during simulation computation.");
		break;

	    case ERRNUMMDL:
		expected = iface->metadata->num_models;
		release_simengine();
		ERROR(Simatra:SIMEX:HELPER:valueError, "Expected to run %d parallel models but received %d.", expected, models);
		break;
	    }

	mexSimengineResult(iface, nlhs, plhs, models, result);

	release_simengine();
      }
    }
