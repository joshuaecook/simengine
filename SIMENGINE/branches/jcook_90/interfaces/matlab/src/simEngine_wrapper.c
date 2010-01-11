#include <stdio.h>
#include <string.h>
#include <mex.h>

// define options
#define CPUTARGET 1
#define OPENMPTARGET 2
#define CUDATARGET 3
#define FLOATPRECISION 1
#define DOUBLEPRECISION 2
#define TRUE 1
#define FALSE 0
struct targetopts {
  char *target;
  char *precision;
  int num_models;
  int debug;
  int profile;
  int emulate;
};

int runsimEngine (char *simengine, char *file, char *modelname, struct targetopts *opts, int verbose)
{
  FILE *fp;
  char readbuffer[1000];
  char cmdline[1000];
  int errored = 1;

  // Settings structure - hold target specific settings
  // target: CPU=1 | OPENMP=2 | CUDA=3
  // precision: FLOAT=1 | DOUBLE=2
  // num_models: int
  // debug: FALSE=0 | TRUE=1
  // profile: FALSE=0 | TRUE=1
  char settings[1000];
  snprintf(settings, 1000, "{target=\\\"%s\\\",precision=\\\"%s\\\",num_models=%d,debug=%s,profile=%s,emulate=%s}", 
	   opts->target, opts->precision, opts->num_models, 
	   opts->debug ? "true" : "false", 
	   opts->profile ? "true" : "false",
	   opts->emulate ? "true" : "false");

  snprintf(cmdline, 1000, "sh -c 'echo \"print(compile2(\\\"%s\\\", %s))\" | %s -batch 2>& 1'", file, settings, simengine);

  /* we must flush because the man page says we should before a popen call */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);
  if (opts->debug)
    mexPrintf("cmdline: %s\n", cmdline);
  fp = popen(cmdline, "r");

  if (fp == NULL)
    {
      mexPrintf("Error launching dsl process\n");
      return -1;
    }

  while (NULL != (fgets(readbuffer, 1000, fp)))
    {
      if (strstr(readbuffer, "Compilation Finished Successfully") != NULL)
	{
	  errored = 0;
	}
      if(verbose)
	mexPrintf("%s", readbuffer);
    }
  if(verbose)
    mexPrintf("\n");
  pclose(fp);

  return errored;
}

void mexFunction(int nlhs __attribute__ ((unused)), mxArray *plhs[ ],int nrhs, const mxArray *prhs[ ]) {
  char simenginecmd[1000], file[1000], modelname[1000];
  char flag[3];
  int verbose;
  int buflen;
  char target[100] = "cpu";
  char precision[100] = "double";
  struct targetopts opts;
  // default opts
  opts.target = target;
  opts.precision = precision;
  opts.num_models = 1;
  opts.debug = FALSE;
  opts.profile = FALSE;

  if (nrhs < 3 || nrhs > 9)
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Arguments are (simEnginecmd, file, modelname [, '-v'])");
    }

  if (mxGetString(prhs[0], simenginecmd, 1000))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "simEngine command argument is not a string");
    }

  if (mxGetString(prhs[1], file, 1000))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "File name argument is not a string");
    }

  if (mxGetString(prhs[2], modelname, 1000))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Model name argument is not a string");
    }
  
  if (nrhs >= 4 && (mxGetString(prhs[3], flag, 3) || (strncmp(flag, "-v", 2) && strncmp(flag, "+v", 2))))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Fourth parameter is not optional flag, '+/-v'");
    }
  verbose = (strcmp(flag, "+v")==0) ? TRUE : FALSE;

  if (nrhs >= 5 && (mxGetString(prhs[4], flag, 3) || (strncmp(flag, "-d", 2) && strncmp(flag, "+d", 2))))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Fifth parameter is not debug, '+/-d'");
    }
  opts.debug = (strcmp(flag, "+d")==0) ? TRUE : FALSE;

  if (nrhs >= 6 && (mxGetString(prhs[5], flag, 3) || (strncmp(flag, "-p", 2) && strncmp(flag, "+p", 2))))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Fifth parameter is not profile, '+/-p'");
    }
  opts.profile = (strcmp(flag, "+p")==0) ? TRUE : FALSE;

  if (nrhs >= 7 && (mxGetString(prhs[6], opts.target, 100)))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Seventh parameter is not the target");
    }

  if (nrhs >= 8 && (mxGetString(prhs[7], opts.precision, 100)))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Eighth parameter is not precision");
    }
  buflen = (mxGetM(prhs[7]) * mxGetN(prhs[7]) * sizeof(mxChar)) + 1;
  mxGetString(prhs[7], opts.precision, buflen);

  if (nrhs >= 9 && !mxIsDouble(prhs[8]))
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Ninth parameter is not num models");
    }
  opts.num_models = mxGetScalar(prhs[8]);


  
  plhs[0] = mxCreateDoubleScalar(runsimEngine(simenginecmd, file, modelname, &opts, verbose));
  
}
/*
int main(int argc, char **argv)
{
  if (argc != 3)
    {
      printf("Usage: %s <dsl file> <modelname> \n", argv[0]);
      return -1;
    }
  printf("Status = %d\n", runSimengine ("./dynamo", argv[1], argv[2]));
}
*/
