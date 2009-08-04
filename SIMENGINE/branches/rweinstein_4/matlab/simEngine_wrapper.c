#include <stdio.h>
#include <string.h>
#include <mex.h>

int runsimEngine (char *simengine, char *file, char *modelname)
{
  FILE *fp;
  char readbuffer[1000];
  char cmdline[1000];

  int errored = 1;

  snprintf(cmdline, 1000, "sh -c 'echo \"import \\\"%s\\\"\nprint(compile(%s))\" | %s -batch 2>& 1'", file, modelname, simengine);

  /* we must flush because the man page says we should before a popen call */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);
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

      mexPrintf("%s", readbuffer);
    }
  mexPrintf("\n");
  pclose(fp);

  return errored;
}

void mexFunction(int nlhs, mxArray *plhs[ ],int nrhs, const mxArray *prhs[ ]) {
  char simenginecmd[1000], file[1000], modelname[1000];
  if (nrhs != 3)
    {
      mexErrMsgIdAndTxt("Simatra:argumentError", "Arguments are (simEnginecmd, file, modelname)");
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

  plhs[0] = mxCreateDoubleScalar(runsimEngine(simenginecmd, file, modelname));
  
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
