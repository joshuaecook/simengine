#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ffi-simengine.h"

/* Vectors and strings returned to SML must be allocated on the heap. */

/* Copies a string to heap-allocated, garbage-collected memory. */
String8_t heap_string (const size_t length, const char *string)
    {
    char *gc_string = (char *)alloc_char(length);
    memcpy(gc_string, string, length * sizeof(char));
    return gc_string;
    }

/* Copies an array of strings to heap-allocated, garbage-collected memory. */
Vector(String8_t) heap_string_array (const size_t length, const char **strings)
    {
    char **gc_array = (char **)alloc_string(length);
    int i;

    for (i = 0; i < length; i++)
	update_string(gc_array, i, heap_string(strlen(strings[i]), strings[i]));

    return gc_array;
    }

/* Copies an array of ints to heap-allocated, garbage-collected memory. */
Vector(Int32_t) heap_int_array (const size_t length, const int *nums)
    {
    double *gc_array = (double *)alloc_int32(length);
    memcpy(gc_array, nums, length * sizeof(double));
    return gc_array;
    }

/* Copies an array of doubles to heap-allocated, garbage-collected memory. */
Vector(Real64_t) heap_double_array (const size_t length, const double *reals)
    {
    double *gc_array = (double *)alloc_real64(length);
    memcpy(gc_array, reals, length * sizeof(double));
    return gc_array;
    }


typedef struct {
  const uint64_t hashcode;
  const uint32_t num_models;
  const uint32_t num_solvers;
  const Vector(String8_t) solvers;
  const String8_t target;
  const size_t precision;
} simengine_metadata_t;

typedef struct{
  const uint32_t version;
  const uint32_t num_iterators;
  const uint32_t num_inputs;
  const uint32_t num_states;
  const uint32_t num_outputs;
  const Vector(String8_t) iterator_names;
  const Vector(String8_t) input_names;
  const Vector(String8_t) state_names;
  const Vector(String8_t) output_names;
  const Vector(Real64_t) default_inputs;
  const Vector(Real64_t) default_states;
  const Vector(uint32_t) output_num_quantities;
  const String8_t name;
  const simengine_metadata_t *metadata;
} simengine_interface_t;



static const char * solvers[] = {"foo","bar"};
static const simengine_metadata_t semeta = {
    UINT64_C(0),
    1,//  NUM_MODELS,
    2,// NUM_SOLVERS,
    solvers,
    "quux",//target,
    sizeof(double)
};

static const double default_inputs[] = {42.0, 3.14, 1.61};
static const double default_states[] = {0.0, 1.0};
static const char * iterator_names[] = {"t"};
static const char * input_names[] = {"x0","x1","x2"};
static const char * state_names[] = {"s0", "s1"};
static const char * output_names[] = {"y0"};
static const size_t output_num_quantities[] = {1};
static const simengine_interface_t seint = {
    1,//VERSION,
    1,//NUM_ITERATORS,
    3,//NUM_INPUTS,
    2,//NUM_STATES,
    1,//NUM_OUTPUTS,
    iterator_names,
    input_names,
    state_names,
    output_names,
    default_inputs,
    default_states,
    output_num_quantities,
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

Vector(String8_t) seint_iterator_names (Pointer iface)
    { 
    size_t length = seint_num_iterators(iface);
    const char **strings = ((simengine_interface_t *)iface)->iterator_names; 
    return heap_string_array(length, strings);
    }

Vector(String8_t) seint_input_names (Pointer iface)
    { 
    size_t length = seint_num_inputs(iface);
    const char **strings = ((simengine_interface_t *)iface)->input_names; 
    return heap_string_array(length, strings);
    }

Vector(String8_t) seint_state_names (Pointer iface)
    { 
    size_t length = seint_num_states(iface);
    const char **strings = ((simengine_interface_t *)iface)->state_names; 
    return heap_string_array(length, strings);
    }

Vector(String8_t) seint_output_names (Pointer iface)
    {
    size_t length = seint_num_outputs(iface);
    const char **strings = ((simengine_interface_t *)iface)->output_names; 
    return heap_string_array(length, strings);
    }

Vector(Real64_t) seint_default_inputs (Pointer iface)
    { 
    size_t length = seint_num_inputs(iface);
    const double *reals = ((simengine_interface_t *)iface)->default_inputs;
    return heap_double_array(length, reals);
    }

Vector(Real64_t) seint_default_states (Pointer iface)
    { 
    size_t length = seint_num_states(iface);
    const double *reals = ((simengine_interface_t *)iface)->default_states;
    return heap_double_array(length, reals);
    }

Vector(Int32_t) seint_output_num_quantities (Pointer iface)
    { 
    size_t length = seint_num_outputs(iface);
    const uint32_t *nums = ((simengine_interface_t *)iface)->output_num_quantities;
    return heap_int_array(length, nums);
    }

String8_t seint_name (Pointer iface)
    {
    const char *name = ((simengine_interface_t *)iface)->name;
    return heap_string(strlen(name), name);
    }

Pointer seint_metadata (Pointer iface)
    { return ((simengine_interface_t *)iface)->metadata; }

Int64_t semeta_hashcode (Pointer meta)
    { return ((simengine_metadata_t *)meta)->hashcode; }

Int32_t semeta_num_models (Pointer meta)
    { return ((simengine_metadata_t *)meta)->num_models; }

Int32_t semeta_num_solvers (Pointer meta)
    { return ((simengine_metadata_t *)meta)->num_solvers; }

Vector(String8_t) semeta_solvers (Pointer meta)
    {
    size_t length = semeta_num_solvers(meta);
    const char *strings = ((simengine_metadata_t *)meta)->solvers;
    return heap_string_array(length, strings);
    }

String8_t semeta_target (Pointer meta)
    {
    const char *target = ((simengine_metadata_t *)meta)->target;
    return heap_string(strlen(target), target);
    }

Int32_t semeta_precision (Pointer meta)
    { return ((simengine_metadata_t *)meta)->precision; }
