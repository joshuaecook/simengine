#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <dlfcn.h>

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

    api->getinterface = (simengine_interface* (*)())dlsym(simengine, "simengine_getinterface");
    if (0 != (msg = dlerror()))
	{ 
	ERROR(Simatra:SIMEX:HELPER:dynamicLoadError, 
	    "dlsym() failed to load getinterface: %s", msg); 
	}
    api->runmodel = (simengine_result* (*)(double, double, unsigned int, double*, double*, simengine_alloc*))dlsym(simengine, "simengine_runmodel");
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


int main(int argc, char **argv)
    {
    unsigned int models = NUM_MODELS, modelid;
    double stop_time = 10.0;

    const char *name = "/home/jcook/SourceCode/simEngine/branches/clebsack_6/local-install/libsimengine.so";
    simengine_api *api = init_simengine(load_simengine(name));
    simengine_interface *iface = api->getinterface();
    simengine_alloc allocator = { MALLOC, REALLOC, FREE };

    double *inputs = NMALLOC(models * iface->num_inputs, double);
    for (modelid = 0; modelid < models; ++modelid)
	{
	memcpy(&inputs[AS_IDX(iface->num_inputs, models, 0, modelid)], iface->default_inputs, iface->num_inputs * sizeof(double));
	}
    double *states = NMALLOC(models * iface->num_states, double);
    for (modelid = 0; modelid < models; ++modelid)
	{
	memcpy(&states[AS_IDX(iface->num_states, models, 0, modelid)], iface->default_states, iface->num_states * sizeof(double));
	}

    simengine_result *result = api->runmodel(0, stop_time, models, inputs, states, &allocator);

    if (0 != result->status)
	{
	ERROR(Simatra:error, "runmodel returned non-zero status %d: %s", result->status, result->status_message);
	}

    simengine_output *output = result->outputs;
    for (unsigned int modelid = 0; modelid < models; ++modelid)
	{
	for (unsigned int outputid = 0; outputid < iface->num_outputs; ++outputid, ++output)
	    {
	    for (unsigned int sampleid = 0; sampleid < output->num_samples; ++sampleid)
		{
		for (unsigned int quantityid = 0; quantityid < output->num_quantities; ++quantityid)
		    {
		    double d = output->data[AS_IDX(output->num_quantities, output->num_samples, quantityid, sampleid)];

		    PRINTF("[%d %d %d %d]\t%f\n", modelid, outputid, sampleid, quantityid, d);
		    }
		}
	    }
	}
    return 0;
    }
