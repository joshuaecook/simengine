// Need to define a storage class even though this code will not be
// manipulating device storage.
#define SIMENGINE_STORAGE_double
#define TARGET_CPU
#include <simengine_api.h>

#include <Python.h>
#include <numpy/arrayobject.h>

#include <dlfcn.h>

/* Initializes the Python module. */
PyMODINIT_FUNC initsimex_helper(void);
/* Python callable: simex_helper.simex_helper(dll, ...) */
static PyObject *simex_helper(PyObject *self, PyObject *args);
static int simex_helper_init_simengine(const char *name, simengine_api *api);
static void simex_helper_release_simengine(simengine_api *api);
static void simex_helper_simengine_results(const simengine_interface *iface, simengine_result *result, PyObject **value);
static void simex_helper_simengine_interface(const simengine_interface *iface, PyObject **value);


static PyObject *simex_doit(PyObject *self, PyObject *args);


/* The method table for this module.
 * It must end with the null sentinel value.
 */
static PyMethodDef SimexHelperMethods[] =
    {
    {"simex_helper", simex_helper, METH_VARARGS, "A helper function for SIMEX."},
//    {"doit", simex_doit, METH_VARARGS, "A helper function for SIMEX."},
    {NULL, NULL, 0, NULL}
    };

PyMODINIT_FUNC initsimex_helper(void)
    {
    import_array();
    Py_InitModule("simex_helper", SimexHelperMethods);
    }

static PyObject *simex_doit(PyObject *self, PyObject *args)
    {
    fprintf(stderr, "self %p\n", self);
    fprintf(stderr, "args %p\n", args);

    PyObject *it = Py_None;
    if (!PyArg_ParseTuple(args, "O!", &PyArray_Type, &it))
	{ Py_RETURN_NONE; }

    if (!PyArray_Check(it))
	{
	PyErr_SetString(PyExc_ValueError, "Expected an array.");
	Py_RETURN_NONE;
	}
    
    fprintf(stderr, "it %p\n", it);

    return it;
    }

static PyObject *simex_helper(PyObject *self, PyObject *args)
    {
    simengine_api api;
    const simengine_interface *iface;
    
    const char *dll;
    double startTime = 0.0, stopTime = 0.0;
    PyObject *userInputs = Py_None, *userStates = Py_None;
    PyObject *value = Py_None;

    simengine_result *result;
    simengine_alloc allocator = { PyMem_Malloc, PyMem_Realloc, PyMem_Free };
    unsigned int num_models;
    PyArrayObject *pyInputs, *pyStates;
    double *inputs, *states;

    if (!PyArg_ParseTuple(args, "s|(dd)O!O!:simex_helper", &dll, 
	    &startTime, &stopTime, 
	    &PyArray_Type, &userInputs, 
	    &PyArray_Type, &userStates))
    	{ Py_RETURN_NONE; }

    if (0 != simex_helper_init_simengine(dll, &api)) 
	{ 
	PyErr_SetString(PyExc_RuntimeError,
	    "Unable to load simEngine DLL.");
	Py_RETURN_NONE;
	}

    iface = api.getinterface();

    if (stopTime <= 0.0)
	{
	simex_helper_simengine_interface(iface, &value);
	if (!(value || PyErr_Occurred()))
	    {
	    PyErr_SetString(PyExc_RuntimeError,
		"Unable to construct simEngine interface.");
	    }

	simex_helper_release_simengine(&api);
	return value;
	}


    if (NULL == userStates || Py_None == userStates || NULL == userInputs || Py_None == userInputs) 
	{
	simex_helper_release_simengine(&api);

	PyErr_SetString(PyExc_ValueError,
	    "Neither INPUTS nor Y0 may be None.");
	Py_RETURN_NONE;
	}
    if (!(PyArray_Check(userStates) && PyArray_Check(userInputs)))
	{
	simex_helper_release_simengine(&api);

	PyErr_SetString(PyExc_ValueError,
	    "INPUTS and Y0 must be Numpy ndarray instances.");
	Py_RETURN_NONE;
	}

    pyInputs = (PyArrayObject *)PyArray_ContiguousFromAny(userInputs, NPY_DOUBLE, 2, 2);
    pyStates = (PyArrayObject *)PyArray_ContiguousFromAny(userStates, NPY_DOUBLE, 2, 2);
	
    num_models = PyArray_DIM(pyInputs, 0);
    if (num_models != PyArray_DIM(pyStates, 0))
	{
	simex_helper_release_simengine(&api);

	PyErr_SetString(PyExc_ValueError,
	    "INPUTS and Y0 must be the same length.");
	Py_RETURN_NONE;

	}
    if (num_models < 1)
	{
	simex_helper_release_simengine(&api);

	PyErr_SetString(PyExc_RuntimeError,
	    "No models can be run.");
	Py_RETURN_NONE;
	}

    inputs = (double *)PyArray_DATA(pyInputs);
    states = (double *)PyArray_DATA(pyStates);

    result = api.runmodel(startTime, stopTime, num_models, inputs, states, &allocator);

    switch (result->status)
	{
	case ERRMEM:
	    PyErr_SetString(PyExc_RuntimeError,
		"Memory error in simulation.");
	    break;

	case ERRCOMP:
	    PyErr_SetString(PyExc_RuntimeError,
		"Computation error in simulation.");
	    break;

	case ERRNUMMDL:
	    PyErr_SetString(PyExc_RuntimeError,
		"Incorrect number of models.");
	    break;

	case SUCCESS:
	    simex_helper_simengine_results(iface, result, &value);
	    break;
	}

    simex_helper_release_simengine(&api);
    return value;
    }


static int simex_helper_init_simengine(const char *name, simengine_api *api)
    {
    char *error;

    api->driver = dlopen(name, RTLD_NOW);
    if (0 != (error = dlerror()))
	{ return 1; }

    api->getinterface = (simengine_getinterface_f)dlsym(api->driver, "simengine_getinterface");
    if (0 != (error = dlerror()))
	{ return 1; }

    api->runmodel = (simengine_runmodel_f)dlsym(api->driver, "simengine_runmodel");
    if (0 != (error = dlerror()))
	{ return 1; }

    api->evalflow = (simengine_evalflow_f)dlsym(api->driver, "simengine_evalflow");
    if (0 != (error = dlerror()))
	{ return 1; }

    return 0;
    }

static void simex_helper_release_simengine(simengine_api *api)
    {
    dlclose(api->driver);
    }

static void simex_helper_simengine_results(const simengine_interface *iface, simengine_result *result, PyObject **value)
    {
    // TODO do the objects created herein need to be decref'd before returning?
    PyObject *outputs, *states, *times;
    unsigned int modelid, outputid, quantityid, stateid;
    unsigned int num_outputs = iface->num_outputs, 
	num_models = iface->metadata->num_models,
	num_states = iface->num_states;
    simengine_output *outp = result->outputs;

    outputs = PyList_New(num_models);
    states = PyList_New(num_models);
    times = PyList_New(num_models);

    for (modelid = 0; modelid < num_models; ++modelid)
	{
	PyObject *model_outputs = PyDict_New();
	for (outputid = 0; outputid < num_outputs; ++outputid)
	    {
	    PyObject *output_quantities = PyList_New(outp->num_quantities);
	    for (quantityid = 0; quantityid < outp->num_quantities; ++quantityid)
		{
		PyList_SetItem(output_quantities, 
		    quantityid, PyFloat_FromDouble(outp->data[quantityid]));
		}
	    PyDict_SetItemString(model_outputs,
		iface->output_names[outputid], output_quantities);
	    ++outp;
	    }
	PyList_SetItem(outputs, modelid, model_outputs);

	PyObject *model_states = PyList_New(num_states);
	for (stateid = 0; stateid < num_states; ++stateid)
	    {
	    PyList_SetItem(model_states, 
		stateid, 
		PyFloat_FromDouble(result->final_states[stateid + (modelid * num_states)]));
	    }
	PyList_SetItem(states, modelid, model_states);

	PyList_SetItem(times, 
	    modelid, PyFloat_FromDouble(result->final_time[modelid]));
	}

    *value = PyList_New(3);
    PyList_SetItem(*value, 0, outputs);
    PyList_SetItem(*value, 1, states);
    PyList_SetItem(*value, 2, times);
    }

static void simex_helper_simengine_interface(const simengine_interface *iface, PyObject **value)
    {
    // TODO do the objects created herein need to be decref'd before returning?
    PyObject *input_names, *state_names, *output_names;
    PyObject *default_inputs, *default_states;
    PyObject *output_num_quantities;
    PyObject *metadata, *interface;
    unsigned int i;

    // Constructs the payload
    input_names = PyList_New(iface->num_inputs);
    state_names = PyList_New(iface->num_states);
    output_names = PyList_New(iface->num_outputs);

    default_inputs = PyDict_New();
    default_states = PyList_New(iface->num_states);

    output_num_quantities = PyList_New(iface->num_outputs);

    for (i = 0; i < iface->num_inputs || i < iface->num_states || i < iface->num_outputs; ++i)
	{
	if (iface->num_inputs > i)
	    {
	    PyList_SetItem(input_names, 
		i, PyString_FromString(iface->input_names[i]));
	    PyDict_SetItemString(default_inputs,
	    	iface->input_names[i], PyFloat_FromDouble(iface->default_inputs[i]));
	    }
	if (iface->num_states > i)
	    {
	    PyList_SetItem(state_names, 
		i, PyString_FromString(iface->state_names[i]));
	    PyList_SetItem(default_states,
	    	i, PyFloat_FromDouble(iface->default_states[i]));
	    }
	if (iface->num_outputs > i)
	    {
	    PyList_SetItem(output_names,
		i, PyString_FromString(iface->output_names[i]));
	    PyList_SetItem(output_num_quantities,
		i, PyLong_FromUnsignedLong(iface->output_num_quantities[i]));
	    }
	}

    metadata = PyDict_New();
    PyDict_SetItemString(metadata,
	"hashcode", PyLong_FromUnsignedLongLong(iface->metadata->hashcode));
    PyDict_SetItemString(metadata,
	"num_models", PyLong_FromUnsignedLong(iface->metadata->num_models));
    PyDict_SetItemString(metadata,
	"solver", PyString_FromString(iface->metadata->solver));
    PyDict_SetItemString(metadata,
	"precision", PyLong_FromUnsignedLong(iface->metadata->precision));

    // Creates and initializes the return dict.
    interface = PyDict_New();
    PyDict_SetItemString(interface, 
	"name", PyString_FromString(iface->name));
    PyDict_SetItemString(interface, 
	"num_inputs", PyLong_FromUnsignedLong(iface->num_inputs));
    PyDict_SetItemString(interface, 
	"num_states", PyLong_FromUnsignedLong(iface->num_states));
    PyDict_SetItemString(interface, 
	"num_outputs", PyLong_FromUnsignedLong(iface->num_outputs));
    PyDict_SetItemString(interface,
	"input_names", input_names);
    PyDict_SetItemString(interface,
	"state_names", state_names);
    PyDict_SetItemString(interface,
	"output_names", output_names);
    PyDict_SetItemString(interface,
	"default_inputs", default_inputs);
    PyDict_SetItemString(interface,
	"default_states", default_states);
    PyDict_SetItemString(interface,
	"output_num_quantities", output_num_quantities);
    PyDict_SetItemString(interface,
	"version", PyLong_FromUnsignedLong(iface->version));
    PyDict_SetItemString(interface,
	"metadata", metadata);

    *value = interface;

    return;
    }


