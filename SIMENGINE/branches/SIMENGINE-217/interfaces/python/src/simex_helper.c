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
      {"doit", simex_doit, METH_VARARGS, "A helper function for SIMEX."},
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
    Py_INCREF(it);

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
    const simengine_interface *iface = NULL;
    
    const char *dll = NULL;
    double startTime = 0.0, stopTime = 0.0;
    PyObject *userInputs = Py_None, *userStates = Py_None;
    PyObject *value = Py_None;

    simengine_result *result = NULL;
    simengine_alloc allocator = { PyMem_Malloc, PyMem_Realloc, PyMem_Free };
    unsigned int num_models = 0;
    PyArrayObject *pyInputs, *pyStates;
    double *inputs = NULL, *states = NULL;

    if (!PyArg_ParseTuple(args, "s|(dd)O!O!:simex_helper", &dll, 
	    &startTime, &stopTime, 
	    &PyArray_Type, &userInputs, 
	    &PyArray_Type, &userStates))
    	{ Py_RETURN_NONE; }

    // Retains a reference to user inputs and states.
    Py_INCREF(userInputs);
    Py_INCREF(userStates);

    if (0 != simex_helper_init_simengine(dll, &api)) 
	{ 
	PyErr_SetString(PyExc_RuntimeError,
	    "Unable to load simEngine API.");
	Py_DECREF(userInputs);
	Py_DECREF(userStates);
	Py_RETURN_NONE;
	}

    iface = api.getinterface();

    if (stopTime <= 0.0)
	{
	simex_helper_simengine_interface(iface, &value);
	if (Py_None == value || PyErr_Occurred())
	    {
	    PyErr_SetString(PyExc_RuntimeError,
		"Unable to construct simEngine interface.");
	    }
	goto simex_helper_return;
	}

    if (Py_None == userStates || Py_None == userInputs) 
	{
	PyErr_SetString(PyExc_ValueError,
	    "Neither INPUTS nor Y0 may be None.");
	goto simex_helper_return;
	}
    if (!(PyArray_Check(userStates) && PyArray_Check(userInputs)))
	{
	PyErr_SetString(PyExc_ValueError,
	    "INPUTS and Y0 must be Numpy ndarray instances.");
	goto simex_helper_return;
	}

    pyInputs = (PyArrayObject *)PyArray_ContiguousFromAny(userInputs, NPY_DOUBLE, 2, 2);
    pyStates = (PyArrayObject *)PyArray_ContiguousFromAny(userStates, NPY_DOUBLE, 2, 2);
	

    num_models = PyArray_DIM(pyInputs, 0);
    if (num_models != PyArray_DIM(pyStates, 0))
	{
	PyErr_SetString(PyExc_ValueError,
	    "INPUTS and Y0 must be the same length.");
	goto simex_helper_return;
	}
    if (num_models < 1)
	{
	PyErr_SetString(PyExc_RuntimeError,
	    "No models can be run.");
	goto simex_helper_return;
	}

    inputs = (double *)PyArray_GetPtr(pyInputs,0);
    inputs = (double *)PyArray_GetPtr(pyStates,0);

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

    // Errored branches may jump here. 
    // Ensures that all resources are reclaimed before returning.
    simex_helper_return:
    Py_DECREF(userInputs);
    Py_DECREF(userStates);
    simex_helper_release_simengine(&api);
    return value;
    }


static int simex_helper_init_simengine(const char *name, simengine_api *api)
    {
    char *error;
    // Loads the simulation library and obtains pointers to its public API functions.

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
    PyObject *outputs, *states, *times;
    unsigned int modelid, outputid;
    unsigned int num_outputs = iface->num_outputs, 
	num_models = iface->metadata->num_models,
	num_states = iface->num_states;
    simengine_output *outp = result->outputs;

    outputs = PyList_New(num_models);

    // Nb. PyList_SetItem() ``steals'' the reference given, but
    // PyDict_SetItemString() ``borrows'' the reference.
    // See the Python C API docs for information on reference semantics.

    for (modelid = 0; modelid < num_models; ++modelid)
	{
	PyObject *model_outputs = PyDict_New();
	for (outputid = 0; outputid < num_outputs; ++outputid)
	    {
	    npy_intp dims[2] = {outp->num_samples, outp->num_quantities};
	    PyObject *output_quantities = PyArray_SimpleNewFromData(2, dims, NPY_DOUBLE, outp->data);

	    PyDict_SetItemString(model_outputs,
		iface->output_names[outputid], output_quantities);
	    Py_DECREF(output_quantities);

	    ++outp;
	    }
	PyList_SetItem(outputs, modelid, model_outputs);
	}

	{
	npy_intp dims[2] = {num_models, num_states};
	states = PyArray_SimpleNewFromData(2, dims, NPY_DOUBLE, result->final_states);
	}

	{
	npy_intp dims[2] = {num_models, 1};
	times = PyArray_SimpleNewFromData(2, dims, NPY_DOUBLE, result->final_time);
	}

    *value = Py_BuildValue("(OOO)", outputs, states, times);
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


