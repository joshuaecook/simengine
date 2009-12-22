#include <stdlib.h>

#include "ffi-simengine.h"

Int32 ffi (Int32 a)
    {
    return 2 * a;
    }

typedef struct {
  const uint64_t hashcode;
  const uint32_t num_models;
  const uint32_t num_solvers;
  const Vector(String32_t) solvers;
  const String32_t target;
  const size_t precision;
} simengine_metadata_t;

typedef struct{
  const uint32_t version;
  const uint32_t num_iterators;
  const uint32_t num_inputs;
  const uint32_t num_states;
  const uint32_t num_outputs;
  const Vector(String32_t) iterator_names;
  const Vector(String32_t) input_names;
  const Vector(String32_t) state_names;
  const Vector(String32_t) output_names;
  const Vector(Real64_t) default_inputs;
  const Vector(Real64_t) default_states;
  const Vector(uint32_t) output_num_quantities;
  const String32_t name;
  const simengine_metadata_t *metadata;
} simengine_interface_t;



static const simengine_metadata_t semeta = {
    UINT64_C(0),
    1,//  NUM_MODELS,
    2,// NUM_SOLVERS,
    {"foo","bar"},//solvers,
    "quux",//target,
    sizeof(double)
};

static const double default_inputs[] = {42.0, 3.14, 1.61};
static const double default_states[] = {0.0, 1.0};
static const simengine_interface_t seint = {
    1,//VERSION,
    1,//NUM_ITERATORS,
    3,//NUM_INPUTS,
    2,//NUM_STATES,
    1,//NUM_OUTPUTS,
    {"t"},//iterator_names,
    {"x0","x1","x2"},//input_names,
    {"s0","s1"},//state_names,
    {"y0"},//output_names,
    default_inputs,
    default_states,
    {1},//output_num_quantities,
    "dillinger",//model_name,
    &semeta
};

Pointer simengine_interface ()
    { return &seint; }

Int32_t seint_version (Pointer iface)
    { return ((simengine_interface_t *)iface)->version; }

Int32_t seint_num_iterators (Pointer iface)
    { return ((simengine_interface_t *)iface)->num_iterators; }

Int32_t seint_num_inputs (Pointer iface)
    { return ((simengine_interface_t *)iface)->num_inputs; }

Int32_t seint_num_states (Pointer iface)
    { return ((simengine_interface_t *)iface)->num_states; }

Int32_t seint_num_outputs (Pointer iface)
    { return ((simengine_interface_t *)iface)->num_outputs; }

Vector(String32_t) seint_iterator_names (Pointer iface)
    { return ((simengine_interface_t *)iface)->iterator_names; }

Vector(String32_t) seint_input_names (Pointer iface)
    { return ((simengine_interface_t *)iface)->input_names; }

Vector(String32_t) seint_state_names (Pointer iface)
    { return ((simengine_interface_t *)iface)->state_names; }

Vector(String32_t) seint_output_names (Pointer iface)
    { return ((simengine_interface_t *)iface)->output_names; }

Vector(Real64_t) seint_default_inputs (Pointer iface)
    { 
    int i, num_inputs = seint_num_inputs(iface);
    double *default_inputs = ((simengine_interface_t *)iface)->default_inputs;
    double *heap_inputs = (double *)alloc_real64(num_inputs, 0.0);

    for (i = 0; i < num_inputs; i++)
	{ heap_inputs[i] = default_inputs[i]; }

    return heap_inputs; 
    }

Vector(Real64_t) seint_default_states (Pointer iface)
    { return ((simengine_interface_t *)iface)->default_states; }

Vector(Int32_t) seint_output_num_quantities (Pointer iface)
    { return ((simengine_interface_t *)iface)->output_num_quantities; }

String32 seint_name (Pointer iface)
    { return ((simengine_interface_t *)iface)->name; }

Pointer seint_metadata (Pointer iface)
    { return ((simengine_interface_t *)iface)->metadata; }

Int64_t semeta_hashcode (Pointer meta)
    { return ((simengine_metadata_t *)meta)->hashcode; }
