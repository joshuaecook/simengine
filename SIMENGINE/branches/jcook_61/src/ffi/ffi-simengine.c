#include <stdlib.h>
#include <string.h>

/* The header should be generated by MLton.
 * It shall declare MLton's own data types
 * as well as the signatures of all
 * exported functions (e.g. those use to
 * allocate memory for arrays.) */
#include "ffi-exports.h"
#include "../../codegen/src/simengine/simengine_api.h"

Vector(Word64_t) seint_vector64 (size_t length, Word64_t *defaults)
    {
    int i;
    Word64_t *words = (Word64_t *)heap_alloc_word64(length, 0);
    for (i = 0; i < length; i++)
    	heap_update_word64(words, i, defaults[i]);
    return words;
    }

Vector(Word32_t) seint_vector32 (size_t length, Word32_t *defaults)
    {
    int i;
    Word32_t *words = (Word32_t *)heap_alloc_word32(length, 0);
    for (i = 0; i < length; i++)
	heap_update_word32(words, i, defaults[i]);
    return words;
    }

/* A vector of 8-bit words is appropriate for MLton's string type. */
Vector(Word8_t) seint_vector8 (size_t length, Word8_t *defaults)
    {
    int i;
    Word8_t *words = (Word8_t *)heap_alloc_word8(length, 0);
    for (i = 0; i < length; i++)
	heap_update_word8(words, i, defaults[i]);
    return words;
    }

/* Returns a vector of strings. */
Vector(String8_t) seint_string_vector (size_t length, char **defaults)
    {
    int i;
    char **strings = (char **)heap_alloc_pointer(length, NULL);
    for (i = 0; i < length; i++)
	strings[i] = seint_vector8(strlen(defaults[i]), defaults[i]);
    return strings;
    }

String8 seint_name (Pointer iface)
    { 
    char *name = ((simengine_interface *)iface)->name;
    size_t length = strlen(name);
    return seint_vector8(length, name);
    }

String8 seint_target (Pointer iface)
    {
    char *name = ((simengine_interface *)iface)->target;
    size_t length = strlen(name);
    return seint_vector8(length, name);
    }

Vector(String8_t) seint_solver_names (Pointer iface)
    {
    size_t length = seint_num_iterators(iface);
    char **solvers = ((simengine_interface *)iface)->solver_names;
    return seint_string_vector(length, solvers);
    }

Vector(String8_t) seint_iterator_names (Pointer iface)
    { 
    size_t length = seint_num_iterators(iface);
    char **iterator_names = ((simengine_interface *)iface)->iterator_names;
    return seint_string_vector(length, iterator_names);
    }

Vector(String8_t) seint_input_names (Pointer iface)
    {
    size_t length = seint_num_inputs(iface);
    char **input_names = ((simengine_interface *)iface)->input_names;
    return seint_string_vector(length, input_names);
    }

Vector(String8_t) seint_output_names (Pointer iface)
    {
    size_t length = seint_num_outputs(iface);
    char **output_names = ((simengine_interface *)iface)->output_names;
    return seint_string_vector(length, output_names);
    }

Vector(Real64_t) seint_default_inputs (Pointer iface)
    {
    size_t length = seint_num_inputs(iface);
    double *default_inputs = ((simengine_interface *)iface)->default_inputs;
    return seint_vector64(length, default_inputs);
    }
 
Vector(Real64_t) seint_default_states (Pointer iface)
    { 
    size_t length = seint_num_states(iface);
    double *default_states = ((simengine_interface *)iface)->default_states;
    return seint_vector64(length, default_states);
    }

Vector(Int32_t) seint_output_num_quantities (Pointer iface)
    { 
    size_t length = seint_num_outputs(iface);
    int *quantities = ((simengine_interface *)iface)->output_num_quantities;
    return seint_vector32(length, quantities);
    }

Int32_t seint_version (Pointer iface)
    { return ((simengine_interface *)iface)->version; }

Int32_t seint_precision (Pointer iface)
    { return ((simengine_interface *)iface)->precision; }

Int32_t seint_parallel_models (Pointer iface)
    { return ((simengine_interface *)iface)->parallel_models; }

Int32_t seint_num_iterators (Pointer iface)
    { return ((simengine_interface *)iface)->num_iterators; }

Int32_t seint_num_inputs (Pointer iface)
    { return ((simengine_interface *)iface)->num_inputs; }

Int32_t seint_num_states (Pointer iface)
    { return ((simengine_interface *)iface)->num_states; }

Int32_t seint_num_outputs (Pointer iface)
    { return ((simengine_interface *)iface)->num_outputs; }

Int64_t seint_hashcode (Pointer iface)
    { return ((simengine_interface *)iface)->hashcode; }

