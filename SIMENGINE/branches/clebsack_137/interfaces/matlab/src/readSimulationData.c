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

typedef enum {
  LOG_OUTPUTS_OK = 0,
  LOG_OUTPUTS_CORRUPT,
  LOG_OUTPUTS_OUT_OF_MEMORY
} log_outputs_status_t;

static struct {
  int running;
  int initialized;
  int request_data;
  int shared_memory;
  log_outputs_status_t log_outputs_status;
  unsigned int buffer_count;
  /* precision = sizeof(CDATAFORMAT), i.e. 4 or 8 */
  unsigned int precision; 
  /* pointer_size = sizeof(void*) in simulation address space */
  unsigned int pointer_size;
  /* dimension of simulation-allocated memory; may be larger than */
  /* actual number of simulated instances */
  unsigned int parallel_models; 
  /* number of instances; denotes the extent of valid data in */
  /* simulation-allocated memory */
  unsigned int num_models;
  unsigned int num_outputs;
  unsigned int num_states;
  char *outputs_dirname;
  char **output_names;
  unsigned int *output_num_quantities;
  pthread_t collector;
} collection_status = {0};



typedef struct{
  unsigned int samples;
  unsigned int allocated;
  double *data;
}output_t;

static output_t *output;

#define BASE_BUFFER_SIZE 1024

#define ERROR(MESSAGE, ARG...) mexErrMsgIdAndTxt("Simatra:SIMEX:readSimulationData", MESSAGE, ##ARG)

#define BUFFER_LEN 100

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

unsigned int iface_pointer_size(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *size = mxGetField(iface, 0, "pointer_size");

  assert(mxDOUBLE_CLASS == mxGetClassID(size));
  return mxGetScalar(size);
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

int options_shared_memory(const mxArray *opts){
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

void check_for_error (void) {
  switch (collection_status.log_outputs_status) {
  case LOG_OUTPUTS_OUT_OF_MEMORY:
    cleanup();
    ERROR("Out of memory.\n");
    break;
  case LOG_OUTPUTS_CORRUPT:
    cleanup();
    ERROR("Corrupt data.\n");
    break;
  }
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

  collection_status.request_data = 1;
  if(collection_status.num_outputs){
    pthread_join(collection_status.collector, NULL);
  }
  collection_status.collector = 0;

  check_for_error();

  output_struct = mxCreateStructMatrix(collection_status.num_models, 1, collection_status.num_outputs, (const char **)collection_status.output_names);

  /* Convert output files to mat format */
  for(modelid=0;modelid<collection_status.num_models;modelid++){
    for(outputid=0;outputid<collection_status.num_outputs;outputid++){
      num_samples = output[modelid * collection_status.num_outputs + outputid].samples;
      if(num_samples > 0){
	/* Allocate mat variable */
	mat_output = mxCreateDoubleMatrix(num_samples, collection_status.output_num_quantities[outputid], mxREAL);

	mat_data = mxGetPr(mat_output);

	copy_transpose_double(mat_data, output[modelid * collection_status.num_outputs + outputid].data, collection_status.output_num_quantities[outputid], num_samples);

	free(output[modelid * collection_status.num_outputs + outputid].data);
	output[modelid * collection_status.num_outputs + outputid].data = NULL;

	/* Assign mat variable to return structure */
	mxDestroyArray(mxGetField(output_struct, modelid, collection_status.output_names[outputid]));
	mxSetField(output_struct, modelid, collection_status.output_names[outputid], mat_output);
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

  output_struct = mxCreateStructMatrix(collection_status.num_models, 1, collection_status.num_outputs, (const char **)collection_status.output_names);

  /* Convert output files to mat format */
  for(modelid=0;modelid<collection_status.num_models;modelid++){
    /* Set up the path to the model instance */
    modelid_dirname(collection_status.outputs_dirname, dirname, modelid);

    for(outputid=0;outputid<collection_status.num_outputs;outputid++){
      /* Create full file name of output */
      sprintf(filename, "%s/outputs/%s", dirname, collection_status.output_names[outputid]);

      /* Get size of data file */
      struct stat filestat;
      int num_samples;
      if(stat(filename, &filestat)){
	/* No file means no output produced */
	num_samples = 0;
      }
      else{
	num_samples = filestat.st_size/(collection_status.output_num_quantities[outputid]*sizeof(double));
      }

      if(num_samples > 0){
	/* Allocate mat variable */
	mat_output = mxCreateDoubleMatrix(num_samples, collection_status.output_num_quantities[outputid], mxREAL);

	/* Read file into mat variable */
	fd = open(filename, O_RDONLY);
	if(-1 == fd){
	  ERROR("Could not open '%s'.", filename);
	}

	mat_data = mxGetPr(mat_output);
	file_data = mmap(NULL, filestat.st_size, PROT_READ, MAP_SHARED, fd, 0);

	copy_transpose_double(mat_data, file_data, collection_status.output_num_quantities[outputid], num_samples);

	/* Assign mat variable to return structure */
	mxDestroyArray(mxGetField(output_struct, modelid, collection_status.output_names[outputid]));
	mxSetField(output_struct, modelid, collection_status.output_names[outputid], mat_output);

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
  final_states = mxCreateDoubleMatrix(collection_status.num_models, collection_status.num_states, mxREAL);

  if(final_states && collection_status.num_states){
    /* Create full pathname for final-states file */
    sprintf(filename, "%s/final-states", collection_status.outputs_dirname);

    /* Open file */
    fd = open(filename, O_RDONLY);
    if(-1 == fd){
      ERROR("Could not open '%s'.", filename);
    }
    /* Memory map file */
    file_data = mmap(NULL, collection_status.num_states*collection_status.num_models*sizeof(double), PROT_READ, MAP_SHARED, fd, 0);
    mat_data = mxGetPr(final_states);

    copy_transpose_double(mat_data, file_data, collection_status.num_states, collection_status.num_models);

    /* Unmap and close file */
    munmap(file_data, collection_status.num_states*collection_status.num_models*sizeof(double));
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
  final_times = mxCreateDoubleMatrix(collection_status.num_models, 1, mxREAL);

  if(collection_status.num_models){
    /* Create full path to final-time file */
    sprintf(filename, "%s/final-time", collection_status.outputs_dirname);

    /* Open final-time file */
    fd = open(filename, O_RDONLY);
    if(-1 == fd){
      ERROR("Could not open '%s'.", filename);
    }

    /* Read final-time data from file */
    if(collection_status.num_models * sizeof(double) != read(fd, (void*)mxGetPr(final_times), collection_status.num_models * sizeof(double))){
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

  if(!collection_status.initialized)
    ERROR("Attempted to read simulation data without initializing.\n");

  /* Read outputs */
  if(collection_status.shared_memory){
    switch (collection_status.log_outputs_status) {
    case LOG_OUTPUTS_CORRUPT:
      ERROR("Output data was corrupted.\n");
      break;
    case LOG_OUTPUTS_OUT_OF_MEMORY:
      ERROR("Ran out of memory while collecting output data.\n");
      break;
    }
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
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN * collection_status.precision));
	     
  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = buf->outputid;
    assert(collection_status.num_outputs > outputid);

    nquantities = buf->num_quantities;
    assert(collection_status.output_num_quantities[outputid] == nquantities);

    if (outputid > collection_status.num_outputs) { return LOG_OUTPUTS_CORRUPT; }
    if (collection_status.output_num_quantities[outputid] != nquantities) { return LOG_OUTPUTS_CORRUPT; }
		 
    out = &output[(modelid_offset+modelid)*collection_status.num_outputs + outputid];
		 
    if (out->samples == out->allocated) {
      out->allocated *= 2;
      out->data = (double*)realloc(out->data, nquantities * out->allocated * sizeof(double));
      if (!out->data)
	{ return LOG_OUTPUTS_OUT_OF_MEMORY; }
    }
		 
    odata = &out->data[out->samples * nquantities];
		 
    /* Copies each element individually for implicit type conversion from float of 'precision' to double. */
    if(collection_status.precision == 4){
      float *fquantities = (float*) buf->quantities;
      for (quantityid = 0; quantityid < nquantities; ++quantityid) {
	odata[quantityid] = fquantities[quantityid];
      }
      buf = (output_buffer_data *)(fquantities + nquantities);
    }
    else /* collection_status.precision == 8 */{
      double *dquantities = (double*) buf->quantities;
      for (quantityid = 0; quantityid < nquantities; ++quantityid) {
	odata[quantityid] = dquantities[quantityid];
      }
      buf = (output_buffer_data *)(dquantities + nquantities);
    }

    ++out->samples;
  }
  ob->available[modelid] = 0;
	     
  return LOG_OUTPUTS_OK;
}


/* The main entry for a "collector" thread.
 * First waits until the simulation process has created a file of
 * tagged output quantities, then allocates a memory-mapped region for
 * the output file. Copies all output data into a global linear buffer
 * in row-major order, while converting from simulation-native storage
 * to double-precision float, and resizing the global region as needed.
 */
void *collect_data(void *arg){
  collection_status.running = 1;

  char buffer_file[PATH_MAX];
  struct stat filestat;
  output_buffer *ob;
  void *raw_buffer;
  int fd;
  unsigned int modelid;
  unsigned int modelid_offset;
  unsigned int buffer_size = (((BUFFER_LEN * collection_status.precision) + 
			       (6 * sizeof(unsigned int)) + 
			       (2 * collection_status.pointer_size)) * 
			      collection_status.parallel_models);
  unsigned int bufferid;
  unsigned int *obid;

  filestat.st_size = 0;
  sprintf(buffer_file, "%s/output_buffer", collection_status.outputs_dirname);

  /* Wait for output buffer to be written to file */
  while(stat(buffer_file, &filestat) || filestat.st_size != collection_status.buffer_count * buffer_size){
    usleep(1000);
    if(!collection_status.initialized) return NULL;
  }

  /* Open and memory map output buffer file */
  fd = open(buffer_file, O_RDWR, S_IRWXU);
  raw_buffer = mmap(NULL, filestat.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);

  /* Initialize output buffer pointers to raw buffer data */
  ob = (output_buffer*)malloc(collection_status.buffer_count*sizeof(output_buffer));
  ob[0].finished = (unsigned int *)raw_buffer;
  ob[0].full = ob[0].finished + collection_status.parallel_models;
  ob[0].count = ob[0].full + collection_status.parallel_models;
  ob[0].available = ob[0].count + collection_status.parallel_models;
  ob[0].modelid_offset = ob[0].available + collection_status.parallel_models;
  ob[0].buffer = (char*)(ob[0].modelid_offset + 2 * collection_status.parallel_models);

  int i;
  for(bufferid=1;bufferid<collection_status.buffer_count;bufferid++){
    ob[bufferid].finished = (unsigned int*)(((char*)ob[bufferid-1].finished) + buffer_size);
    ob[bufferid].full = (unsigned int*)(((char*)ob[bufferid-1].full) + buffer_size);
    ob[bufferid].count = (unsigned int*)(((char*)ob[bufferid-1].count) + buffer_size);
    ob[bufferid].available = (unsigned int*)(((char*)ob[bufferid-1].available) + buffer_size);
    ob[bufferid].modelid_offset = (unsigned int*)(((char*)ob[bufferid-1].modelid_offset) + buffer_size);
    ob[bufferid].buffer = ob[bufferid-1].buffer + buffer_size;
  }

  obid = malloc(collection_status.parallel_models * sizeof(unsigned int));
  for(bufferid=0;bufferid<collection_status.parallel_models;bufferid++){
    obid[bufferid] = 0;
  }

  /* Collect data */
  while(collection_status.initialized){
    int logged_something = 0;
    for(modelid=0;modelid<collection_status.parallel_models;modelid++){
      /* There may be more buffers than there are remaining running models, so skip the unused buffers */
      if(ob[obid[modelid]].modelid_offset[modelid] + modelid >= collection_status.num_models) break;

      /* If the buffer has data available, log it */
      if(ob[obid[modelid]].available[modelid] && ob[obid[modelid]].count[modelid]){
	collection_status.log_outputs_status = log_outputs(&ob[obid[modelid]], modelid);
	if (LOG_OUTPUTS_OK != collection_status.log_outputs_status) {
	  goto endofthread;
	}
	logged_something = 1;
	obid[modelid] = (obid[modelid] + 1) % collection_status.buffer_count;
	if (LOG_OUTPUTS_OK != collection_status.log_outputs_status) goto endofthread;
      }
      if(!collection_status.initialized) goto endofthread;
    }
    if(!logged_something){
      if(collection_status.request_data) goto endofthread;
      usleep(10);
    }
  }

 endofthread:
  /* Close output buffer file */
  munmap(raw_buffer, filestat.st_size);
  free(ob);
  free(obid);
  close(fd);
  collection_status.running = 0;

  return NULL;
}

void initialize(const mxArray **prhs){
  /* mexFunction parameters */
  const mxArray *iface = prhs[0];
  const mxArray *options = prhs[1];

  if(!collection_status.initialized){
    collection_status.initialized = 1;

    collection_status.log_outputs_status = LOG_OUTPUTS_OK;
    collection_status.request_data = 0;
    /* Initialize parameter sub fields */
    collection_status.outputs_dirname = options_outputs_directory(options);
    collection_status.num_models = options_instances(options);
    collection_status.shared_memory = options_shared_memory(options);
    collection_status.buffer_count = options_buffer_count(options);
    collection_status.num_outputs = iface_num_outputs(iface);
    collection_status.num_states = iface_num_states(iface);
    collection_status.precision = iface_precision(iface);
    collection_status.pointer_size = iface_pointer_size(iface);
    collection_status.parallel_models = iface_parallel_models(iface);
    if(collection_status.num_outputs){
      collection_status.output_names = iface_outputs(iface);
      collection_status.output_num_quantities = iface_output_num_quantities(iface);
    }
    else{
      collection_status.output_names = NULL;
      collection_status.output_num_quantities = NULL;
    }
    if(collection_status.shared_memory && collection_status.num_outputs){
      unsigned int modelid;
      unsigned int outputid;
      output = (output_t*)malloc(collection_status.num_models * collection_status.num_outputs * sizeof(output_t));
      if(!output){
	ERROR("Out of memory.\n");
      }
      for(modelid=0;modelid<collection_status.num_models;modelid++){
	for(outputid=0;outputid<collection_status.num_outputs;outputid++){
	  output[modelid*collection_status.num_outputs + outputid].data = (double*)malloc(BASE_BUFFER_SIZE * sizeof(double) * collection_status.output_num_quantities[outputid]);
	  if(!output[modelid*collection_status.num_outputs + outputid].data){
	    ERROR("Out of memory.\n");
	  }
	  output[modelid*collection_status.num_outputs + outputid].allocated = BASE_BUFFER_SIZE;
	  output[modelid*collection_status.num_outputs + outputid].samples = 0;
	}
      }

      pthread_attr_t attr;
      pthread_attr_init(&attr);
      pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_JOINABLE);
      if (0 != pthread_create(&collection_status.collector, &attr, collect_data, NULL)) {
	ERROR("Unable to create collection thread.\n");
      }
      pthread_attr_destroy(&attr);

      usleep(1000);
      if (!collection_status.running) {
	ERROR("Collector didn't run.\n");
      }
    }
  }

  check_for_error();

  /* Sleep for 0.1 seconds */
  usleep(1e5);
}

void cleanup(){
  unsigned int modelid;
  unsigned int outputid;

  if(collection_status.initialized){
    /* Signal thread to stop */
    collection_status.initialized = 0;
    if(collection_status.shared_memory && collection_status.num_outputs){
      if(collection_status.collector){
	pthread_join(collection_status.collector, NULL);
      }
      /* Free allocated internal memory */
      if(output){
	for(modelid=0;modelid<collection_status.num_models;modelid++){
	  for(outputid=0;outputid<collection_status.num_outputs;outputid++){
	    if(output[modelid * collection_status.num_outputs + outputid].data){
	      free(output[modelid * collection_status.num_outputs + outputid].data);
	    }
	  }
	}
	free(output);
	output = NULL;
      }
    }
    
    /* Free allocated MATLAB memory */
    if(collection_status.outputs_dirname){
      mxFree(collection_status.outputs_dirname);
      collection_status.outputs_dirname = NULL;
    }
    if(collection_status.output_names){
      for(outputid=0;outputid<collection_status.num_outputs;outputid++){
	if(collection_status.output_names[outputid]){
	  mxFree(collection_status.output_names[outputid]);
	}
      }
      mxFree(collection_status.output_names);
      collection_status.output_names = NULL;
    }
    if(collection_status.output_num_quantities){
      mxFree(collection_status.output_num_quantities);
      collection_status.output_num_quantities = NULL;
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
