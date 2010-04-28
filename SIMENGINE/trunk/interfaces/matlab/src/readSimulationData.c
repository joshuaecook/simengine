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
#include "mex.h"

#define ERROR(MESSAGE, ARG...) mexErrMsgIdAndTxt("Simatra:SIMEX:readSimulationData", MESSAGE, ##ARG)

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
  unsigned int num_outputs = iface_num_outputs(iface);
  mxArray *num_quant = mxGetField(iface, 0, "outputNumQuantities");
  assert(mxDOUBLE_CLASS == mxGetClassID(num_quant));
  
  double *quants = mxGetPr(num_quant);
  unsigned int *num_quants = (unsigned int *)mxMalloc(num_outputs * sizeof(unsigned int));
  unsigned int outputid;
  
  for (outputid = 0; outputid < num_outputs; outputid++){
    num_quants[outputid] = (unsigned int)(quants[outputid]);
  }
  
  return num_quants;
}

/* Returns a vector comprising the name of each output represented in a model interface.
 * Nb caller is responsible for freeing each member *and* the returned pointer by calling mxFree(). */
char **iface_outputs(const mxArray *iface){
  assert(mxSTRUCT_CLASS == mxGetClassID(iface));
  mxArray *outputs = mxGetField(iface, 0, "outputs");
  
  assert(mxCELL_CLASS == mxGetClassID(outputs));
  unsigned int num_outputs = mxGetN(outputs);

  mxArray *name;
  char **names = (char **)mxMalloc(num_outputs * sizeof(char *));
  unsigned int outputid;

  for (outputid = 0; outputid < num_outputs; outputid++){
    name = mxGetCell(outputs, outputid);
    assert(mxCHAR_CLASS == mxGetClassID(name));
    names[outputid] = mxArrayToString(name);
  }

  return names;
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
  if(!dir){
    ERROR("Invalid output directory name.\n");
  }
  return dir;
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

/* Main entry point for readSimulation.mex */
void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]){
  /* Check arguments */
  if(nrhs != 2){
    ERROR("Incorrect number of arguments, expects 2, (interface, options), received %d.\n", nrhs);
  }
  if(nlhs != 3){
    ERROR("Incorrect number of return arguments, expects 3, (outputs, states, times), received %d.\n", nlhs);
  }

  /* mexFunction parameters */
  const mxArray *iface = prhs[0];
  const mxArray *options = prhs[1];
  /* parameter sub fields */
  char *outputs_dirname = options_outputs_directory(options);
  unsigned int modelid_offset = 0;
  unsigned int num_models = options_instances(options);
  unsigned int num_outputs = iface_num_outputs(iface);
  unsigned int num_states = iface_num_states(iface);
  char **output_names = iface_outputs(iface);
  unsigned int *output_num_quantities = iface_output_num_quantities(iface);

  /* Iterators */
  int modelid;
  int outputid;
  int stateid;

  mxArray *output_struct;
  mxArray *final_states;
  mxArray *final_times;
  mxArray *mat_output;

  output_struct = mxCreateStructMatrix(num_models, 1, num_outputs, (const char **)output_names);
  final_states = mxCreateDoubleMatrix(num_models, num_states, mxREAL);
  final_times = mxCreateDoubleMatrix(num_models, 1, mxREAL);

  /* Convert output files to mat format */
  for(modelid=modelid_offset;modelid<modelid_offset+num_models;modelid++){
    int fd;
    char filename[PATH_MAX];
    char dirname[PATH_MAX];
    double *mat_data;
    double *file_data;

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
    /* Read final states */
    if(num_states){
      sprintf(filename, "%s/final-states", dirname);
      fd = open(filename, O_RDONLY);
      if(-1 == fd){
	ERROR("Could not open '%s'.", filename);
      }
      mat_data = mxGetPr(final_states);
      for(stateid=0;stateid<num_states;stateid++){
	if(sizeof(double) != read(fd, &mat_data[(stateid*num_models) + modelid], sizeof(double))){
	  ERROR("Could not read '%d' of '%d' final states from model %d.\n", stateid, num_states, modelid);
	}
      }
      close(fd);
    }

    /* Read the final time */
    sprintf(filename, "%s/final-time", dirname);
    fd = open(filename, O_RDONLY);
    if(-1 == fd){
      ERROR("Could not open '%s'.", filename);
    }
    mat_data = mxGetPr(final_times) + modelid;
    if(sizeof(double) != read(fd, mat_data, sizeof(double))){
      ERROR("Could not read final time from model %d.\n", modelid);
    }
    close(fd);
  }

  /* Free allocated memory */
  mxFree(outputs_dirname);
  for(outputid=0;outputid<num_outputs;outputid++){
    mxFree(output_names[outputid]);
  }
  mxFree(output_names);
  mxFree(output_num_quantities);

  /* Assign return values */
  plhs[0] = output_struct;
  plhs[1] = final_states;
  plhs[2] = final_times;
}


