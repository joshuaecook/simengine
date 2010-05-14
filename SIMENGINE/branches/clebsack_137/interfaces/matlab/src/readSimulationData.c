/* readSimulationData.c
 *
 * Copyright (C) 2010 Simatra Modeling Technologies
 */

#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<sys/types.h>
#include<sys/stat.h>
#include<fcntl.h>
#include<unistd.h>
#include<sys/mman.h>
#include<assert.h>
#include<pthread.h>
#include "mex.h"

/* Library static globals */
static int initialized = 0;
static char *outputs_dirname = NULL;
static unsigned int precision = 0;
static unsigned int parallel_models = 0;
static unsigned int shared_memory = 0;
static unsigned int num_models = 0;
static unsigned int num_outputs = 0;
static unsigned int num_states = 0;
static char **output_names = NULL;
static unsigned int *output_num_quantities = NULL;
static pthread_t collector;

typedef struct{
  unsigned int samples;
  unsigned int allocated;
  double *data;
}output_t;

static output_t *output;

#define BASE_BUFFER_SIZE 1024

#define ERROR(MESSAGE, ARG...) mexErrMsgIdAndTxt("Simatra:SIMEX:readSimulationData", MESSAGE, ##ARG)

unsigned int global_ob_count = 2;
#define BUFFER_LEN 1000

typedef struct{
  unsigned int *finished;
  unsigned int *full;
  unsigned int *count;
  void *buffer;
  unsigned int *available;
  unsigned int *modelid_offset;
}output_buffer;

typedef struct {
  unsigned int outputid;
  unsigned int num_quantities;
  char quantities[];
} output_buffer_data;

/* Returns the number of outputs represented in a model interface. */
unsigned int iface_num_outputs(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *num_quant = mxGetField(iface, 0, "outputNumQuantities");
  
  size_t cols = mxGetN(num_quant);
  return (unsigned int)cols;
}

/* Returns the number of outputs represented in a model interface. */
unsigned int iface_num_states(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *states = mxGetField(iface, 0, "defaultStates");
  
  size_t cols = mxGetN(states);
  return (unsigned int)cols;
}

/* Returns a vector comprising the number of quantities for each output represented in a model interface.
 * Nb caller is responsible for releasing the returned pointer by calling mxFree(). */
unsigned int *iface_output_num_quantities(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  unsigned int num_out = iface_num_outputs(iface);
  mxArray *num_quant = mxGetField(iface, 0, "outputNumQuantities");
  assert(mxDOUBLE_CLASS == mxGetClassID(num_quant));
  
  double *quants = mxGetPr(num_quant);
  unsigned int *num_quants = (unsigned int *)mxMalloc(num_out * sizeof(unsigned int));
  mexMakeMemoryPersistent(num_quants);
  unsigned int outputid;
  
  for (outputid = 0; outputid < num_out; outputid++){
    num_quants[outputid] = (unsigned int)(quants[outputid]);
  }
  
  return num_quants;
}

/* Returns a vector comprising the name of each output represented in a model interface.
 * Nb caller is responsible for freeing each member *and* the returned pointer by calling mxFree(). */
char **iface_outputs(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *out = mxGetField(iface, 0, "outputs");
  
  assert(mxCELL_CLASS == mxGetClassID(out));
  unsigned int num_out = mxGetN(out);

  mxArray *name;
  char **names = (char **)mxMalloc(num_out * sizeof(char *));
  mexMakeMemoryPersistent(names);
  unsigned int outputid;

  for (outputid = 0; outputid < num_out; outputid++){
    name = mxGetCell(out, outputid);
    assert(mxCHAR_CLASS == mxGetClassID(name));
    names[outputid] = mxArrayToString(name);
    mexMakeMemoryPersistent(names[outputid]);
  }

  return names;
}

unsigned int iface_precision(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *prec = mxGetField(iface, 0, "precision");

  assert(mxDOUBLE_CLASS == mxGetClassID(prec));
  return mxGetScalar(prec);
}

unsigned int iface_parallel_models(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *pmodels = mxGetField(iface, 0, "parallel_models");

  assert(mxDOUBLE_CLASS == mxGetClassID(pmodels));
  return mxGetScalar(pmodels);
}

/* Returns the number of parallel instances represented in an options structure. */
unsigned int options_instances(const mxArray *opts){
  assert(mxSTRUCT_CLASS == mxGetClassID(opts));
  mxArray *instances = mxGetField(opts, 0, "instances");

  assert(mxDOUBLE_CLASS == mxGetClassID(instances));
  return mxGetScalar(instances);
}

char *options_outputs_directory(const mxArray *opts){
  char *dir;
  assert(mxSTRUCT_CLASS == mxGetClassID(opts));
  mxArray *outputs = mxGetField(opts, 0, "outputs");

  assert(mxCHAR_CLASS == mxGetClassID(outputs));
  dir = mxArrayToString(outputs);
  mexMakeMemoryPersistent(dir);
  if(!dir){
    ERROR("Invalid output directory name.\n");
  }
  return dir;
}

unsigned int options_shared_memory(const mxArray *opts){
  assert(mxSTRUCT_CLASS == mxGetClassID(opts));
  mxArray *shmemory = mxGetField(opts, 0, "shared_memory");

  assert(mxLOGICAL_CLASS == mxGetClassID(shmemory));
  return mxGetScalar(shmemory);
}

unsigned int options_buffer_count(const mxArray *opts){
  assert(mxSTRUCT_CLASS == mxGetClassID(opts));
  mxArray *buffcount = mxGetField(opts, 0, "buffer_count");

  assert(mxDOUBLE_CLASS == mxGetClassID(buffcount));
  return mxGetScalar(buffcount);
}

/* Takes a base directory and a modelid to create the path unique to the model instance */
void modelid_dirname(const char *outputs_dirname, char *model_dirname, unsigned int modelid){
#define BYTE(val,n) ((val>>(n<<3))&0xff)
  int i;
  sprintf(model_dirname, "%s", outputs_dirname);
  for(i=2;i>=0;i--){
    sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(modelid, i));
  }
#undef BYTE
}

/* Copy data from row major order to column major order. */
void copy_transpose_double(double *dest, const double *src, int cols, int rows){
#define COL_MAJOR_IDX (c*rows + r)
#define ROW_MAJOR_IDX (r*cols + c)

  int c,r;

  /* TODO: see if running this loop multiple times on row blocks equal to page size helps
   * speed things up. */

  for(c=0;c<cols;c++){
    for(r=0;r<rows;r++){
      dest[COL_MAJOR_IDX] = src[ROW_MAJOR_IDX];
    }
  }

#undef COL_MAJOR_IDX
#undef ROW_MAJOR_IDX
}

mxArray *read_outputs_from_shared_memory(){
  /* Local variables */
  mxArray *mat_output;
  double *mat_data;
  double *file_data;
  unsigned int modelid;
  unsigned int outputid;
  unsigned int num_samples;

  /* Return value */
  mxArray *output_struct;

  output_struct = mxCreateStructMatrix(num_models, 1, num_outputs, (const char **)output_names);

  /* Convert output files to mat format */
  for(modelid=0;modelid<num_models;modelid++){
    for(outputid=0;outputid<num_outputs;outputid++){
      num_samples = output[modelid * num_outputs + outputid].samples;
      if(num_samples > 0){
	/* Allocate mat variable */
	mat_output = mxCreateDoubleMatrix(num_samples, output_num_quantities[outputid], mxREAL);

	mat_data = mxGetPr(mat_output);

	copy_transpose_double(mat_data, output[modelid * num_outputs + outputid].data, output_num_quantities[outputid], num_samples);

	free(output[modelid * num_outputs + outputid].data);
	output[modelid * num_outputs + outputid].data = NULL;

	/* Assign mat variable to return structure */
	mxDestroyArray(mxGetField(output_struct, modelid, output_names[outputid]));
	mxSetField(output_struct, modelid, output_names[outputid], mat_output);
      }
    }
  }

  return output_struct;
}

mxArray *read_outputs_from_files(){
  /* Local variables */
  int fd;
  char filename[PATH_MAX];
  char dirname[PATH_MAX];
  mxArray *mat_output;
  double *mat_data;
  double *file_data;
  unsigned int modelid;
  unsigned int outputid;

  /* Return value */
  mxArray *output_struct;

  output_struct = mxCreateStructMatrix(num_models, 1, num_outputs, (const char **)output_names);

  /* Convert output files to mat format */
  for(modelid=0;modelid<num_models;modelid++){
    /* Set up the path to the model instance */
    modelid_dirname(outputs_dirname, dirname, modelid);

    for(outputid=0;outputid<num_outputs;outputid++){
      /* Create full file name of output */
      sprintf(filename, "%s/outputs/%s", dirname, output_names[outputid]);

      /* Get size of data file */
      struct stat filestat;
      int num_samples;
      if(stat(filename, &filestat)){
	/* No file means no output produced */
	num_samples = 0;
      }
      else{
	num_samples = filestat.st_size/(output_num_quantities[outputid]*sizeof(double));
      }

      if(num_samples > 0){
	/* Allocate mat variable */
	mat_output = mxCreateDoubleMatrix(num_samples, output_num_quantities[outputid], mxREAL);

	/* Read file into mat variable */
	fd = open(filename, O_RDONLY);
	if(-1 == fd){
	  ERROR("Could not open '%s'.", filename);
	}

	mat_data = mxGetPr(mat_output);
	file_data = mmap(NULL, filestat.st_size, PROT_READ, MAP_SHARED, fd, 0);

	copy_transpose_double(mat_data, file_data, output_num_quantities[outputid], num_samples);

	/* Assign mat variable to return structure */
	mxDestroyArray(mxGetField(output_struct, modelid, output_names[outputid]));
	mxSetField(output_struct, modelid, output_names[outputid], mat_output);

	/* Unmap and close file */
	munmap(file_data, filestat.st_size);
	close(fd);
      }
    }
  }

  return output_struct;
}

/* Read final states from file */
mxArray *read_final_states(){
  /* Local variables */
  int fd;
  char filename[PATH_MAX];
  double *mat_data;
  double *file_data;
  /* Return value */
  mxArray *final_states;

  /* Allocate final_states matrix */
  final_states = mxCreateDoubleMatrix(num_models, num_states, mxREAL);

  if(num_states && num_states){
    /* Create full pathname for final-states file */
    sprintf(filename, "%s/final-states", outputs_dirname);

    /* Open file */
    fd = open(filename, O_RDONLY);
    if(-1 == fd){
      ERROR("Could not open '%s'.", filename);
    }
    /* Memory map file */
    file_data = mmap(NULL, num_states*num_models*sizeof(double), PROT_READ, MAP_SHARED, fd, 0);
    mat_data = mxGetPr(final_states);

    copy_transpose_double(mat_data, file_data, num_states, num_models);

    /* Unmap and close file */
    munmap(file_data, num_states*num_models*sizeof(double));
    close(fd);
  }

  return final_states;
}

/* Retrieve the final times for all model instances from file */
mxArray *read_final_times(){
  /* Local variables */
  int fd;
  char filename[PATH_MAX];
  double *mat_data;
  /* Return value */
  mxArray *final_times;

  /* Allocate final_times matrix */
  final_times = mxCreateDoubleMatrix(num_models, 1, mxREAL);

  if(num_models){
    /* Create full path to final-time file */
    sprintf(filename, "%s/final-time", outputs_dirname);

    /* Open final-time file */
    fd = open(filename, O_RDONLY);
    if(-1 == fd){
      ERROR("Could not open '%s'.", filename);
    }

    /* Read final-time data from file */
    if(num_models * sizeof(double) != read(fd, (void*)mxGetPr(final_times), num_models * sizeof(double))){
      ERROR("Could not read final time from models.\n");
    }

    /* Close file */
    close(fd);
  }

  return final_times;
}

void return_simulation_data(mxArray **plhs){
  /* Return values */
  mxArray *output_struct;
  mxArray *final_states;
  mxArray *final_times;

  if(!initialized)
    ERROR("Attempted to read simulation data without initializing.\n");

  /* Read outputs */
  if(shared_memory){
    output_struct = read_outputs_from_shared_memory();
  }
  else{
    output_struct = read_outputs_from_files();
  }

  /* Read final states */
  final_states = read_final_states();

  /* Read final times */
  final_times = read_final_times();

  /* Assign return values */
  plhs[0] = output_struct;
  plhs[1] = final_states;
  plhs[2] = final_times;
}

/* Transmutes the internal data buffer into the structured output
 * which may be retured to the client.
 */
int log_outputs(output_buffer *ob, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  output_t *out;
  double *odata;

  unsigned int modelid_offset = ob->modelid_offset[modelid];
  unsigned int ndata = ob->count[modelid];
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN * precision));
	     
  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = buf->outputid;
    assert(num_outputs > outputid);

    nquantities = buf->num_quantities;
    assert(output_num_quantities[outputid] == nquantities);

    /* TODO an error code for invalid data? */
    if (outputid > num_outputs) { return 1; }
    if (output_num_quantities[outputid] != nquantities) { return 1; }
		 
    out = &output[(modelid_offset+modelid)*num_outputs + outputid];
		 
    if (out->samples == out->allocated) {
      out->allocated *= 2;
      out->data = (double*)realloc(out->data, nquantities * out->allocated * sizeof(double));
      if (!out->data)
	{ return 1; }
    }
		 
    odata = &out->data[out->samples * nquantities];
		 
    /* Copies each element individually for implicit type conversion from float of 'precision' to double. */
    if(precision == 4){
      float *fquantities = (float*) buf->quantities;
      for (quantityid = 0; quantityid < nquantities; ++quantityid) {
	odata[quantityid] = fquantities[quantityid];
      }
      buf = (output_buffer_data *)(fquantities + nquantities);
    }
    else /* precision == 8 */{
      double *dquantities = (double*) buf->quantities;
      for (quantityid = 0; quantityid < nquantities; ++quantityid) {
	odata[quantityid] = dquantities[quantityid];
      }
      buf = (output_buffer_data *)(dquantities + nquantities);
    }

    ++out->samples;
  }
  ob->available[modelid] = 0;
	     
  return 0;
}


void *collect_data(void *arg){
  char buffer_file[PATH_MAX];
  struct stat filestat;
  output_buffer *ob;
  void *raw_buffer;
  int fd;
  unsigned int modelid;
  unsigned int modelid_offset;
  unsigned int buffer_size = (((BUFFER_LEN * precision) + 
			       (5 * sizeof(unsigned int)) + 
			       (2 * sizeof(void*))) * 
			      parallel_models);
  unsigned int bufferid;
  unsigned int *obid;

  filestat.st_size = 0;
  sprintf(buffer_file, "%s/output_buffer", outputs_dirname);

  /* Wait for output buffer to be written to file */
  while(stat(buffer_file, &filestat) || filestat.st_size != global_ob_count * buffer_size){
    usleep(1000);
    if(!initialized) return NULL;
  }

  /* Open and memory map output buffer file */
  fd = open(buffer_file, O_RDWR, S_IRWXU);
  raw_buffer = mmap(NULL, filestat.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);

  /* Initialize output buffer pointers to raw buffer data */
  ob = (output_buffer*)malloc(global_ob_count*sizeof(output_buffer));
  ob[0].finished = (unsigned int *)raw_buffer;
  ob[0].full = ob[0].finished + parallel_models;
  ob[0].count = ob[0].full + parallel_models;
  ob[0].available = ob[0].count + parallel_models;
  ob[0].modelid_offset = ob[0].available + parallel_models;
  ob[0].buffer = (char*)(ob[0].modelid_offset + parallel_models);

  int i;
  for(bufferid=1;bufferid<global_ob_count;bufferid++){
    ob[bufferid].finished = (unsigned int*)(((char*)ob[bufferid-1].finished) + buffer_size);
    ob[bufferid].full = (unsigned int*)(((char*)ob[bufferid-1].full) + buffer_size);
    ob[bufferid].count = (unsigned int*)(((char*)ob[bufferid-1].count) + buffer_size);
    ob[bufferid].available = (unsigned int*)(((char*)ob[bufferid-1].available) + buffer_size);
    ob[bufferid].modelid_offset = (unsigned int*)(((char*)ob[bufferid-1].modelid_offset) + buffer_size);
    ob[bufferid].buffer = (unsigned int*)(((char*)ob[bufferid-1].buffer) + buffer_size);
  }

  obid = malloc(parallel_models * sizeof(unsigned int));
  for(bufferid=0;bufferid<parallel_models;bufferid++){
    obid[bufferid] = 0;
  }

  /* Collect data */
  while(initialized){
    for(modelid=0;modelid<parallel_models;modelid++){
      if(ob[obid[modelid]].available[modelid]){
	log_outputs(&ob[obid[modelid]], modelid);
	obid[modelid] = (obid[modelid] + 1) % global_ob_count;
      }
      else{
	usleep(10);
      }
      if(!initialized) goto endofthread;
    }
  }

 endofthread:
  /* Close output buffer file */
  munmap(raw_buffer, filestat.st_size);
  free(ob);
  free(obid);
  close(fd);

  return NULL;
}

void initialize(const mxArray **prhs){
  /* mexFunction parameters */
  const mxArray *iface = prhs[0];
  const mxArray *options = prhs[1];

  if(!initialized){
    /* Initialize parameter sub fields */
    outputs_dirname = options_outputs_directory(options);
    num_models = options_instances(options);
    shared_memory = options_shared_memory(options);
    global_ob_count = options_buffer_count(options);
    num_outputs = iface_num_outputs(iface);
    num_states = iface_num_states(iface);
    precision = iface_precision(iface);
    parallel_models = iface_parallel_models(iface);
    if(num_outputs){
      output_names = iface_outputs(iface);
      output_num_quantities = iface_output_num_quantities(iface);
    }
    else{
      output_names = NULL;
      output_num_quantities = NULL;
    }
    if(shared_memory){
      unsigned int modelid;
      unsigned int outputid;
      output = (output_t*)malloc(num_models * num_outputs * sizeof(output_t));
      if(!output){
	ERROR("Out of memory.\n");
      }
      for(modelid=0;modelid<num_models;modelid++){
	for(outputid=0;outputid<num_outputs;outputid++){
	  output[modelid*num_outputs + outputid].data = (double*)malloc(BASE_BUFFER_SIZE * sizeof(double) * output_num_quantities[outputid]);
	  if(!output[modelid*num_outputs + outputid].data){
	    ERROR("Out of memory.\n");
	  }
	  output[modelid*num_outputs + outputid].allocated = BASE_BUFFER_SIZE;
	  output[modelid*num_outputs + outputid].samples = 0;
	}
      }
      pthread_create(&collector, NULL, collect_data, NULL);
    }
    initialized = 1;
  }

  /* Sleep for 0.1 seconds */
  usleep(1e5);
}

void cleanup(){
  unsigned int modelid;
  unsigned int outputid;

  if(initialized){
    /* Signal thread to stop */
    initialized = 0;
    if(shared_memory){
      pthread_join(collector, NULL);
      /* Free allocated internal memory */
      if(output){
	for(modelid=0;modelid<num_models;modelid++){
	  for(outputid=0;outputid<num_outputs;outputid++){
	    if(output[modelid * num_outputs + outputid].data){
	      free(output[modelid * num_outputs + outputid].data);
	    }
	  }
	}
	free(output);
	output = NULL;
      }
    }
    
    /* Free allocated MATLAB memory */
    if(outputs_dirname){
      mxFree(outputs_dirname);
      outputs_dirname = NULL;
    }
    if(output_names){
      for(outputid=0;outputid<num_outputs;outputid++){
	if(output_names[outputid]){
	  mxFree(output_names[outputid]);
	}
      }
      mxFree(output_names);
      output_names = NULL;
    }
    if(output_num_quantities){
      mxFree(output_num_quantities);
      output_num_quantities = NULL;
    }
  }
}

/* Main entry point for readSimulationData.mex */
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]){
  /* This function has data that is persistent across calls and is overloaded to provide different operations.
   * This routine only dispatches to the main routines for each context */
  if(nlhs == 0 && nrhs == 2){
    /* Initialization and streaming data collection */
    initialize(prhs);
    return;
  }
  if(nlhs == 3 && nrhs == 0){
    /* Return data */
    return_simulation_data(plhs);
    return;
  }
  if(nlhs == 0 && nrhs == 0){
    /* Free persistent data */
    cleanup();
    return;
  }
  ERROR("Incorrect usage of readSimulationData with %d incoming arguments and %d return arguments.\n", nrhs, nlhs);
}