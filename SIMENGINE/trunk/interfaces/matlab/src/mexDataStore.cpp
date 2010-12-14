#include<iostream>
#include<fstream>
#include"DataStore.pb.h"
#include"mex.h"

using namespace std;

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]){
  int i,j,k;
  char *filename;
  char *precision;
  DataGroup::DataType prec;
  const char *fieldname;
  const mxArray *matstruct, *matcell, *matfield;
  mwSize numfields;
  mwSize ndims;
  mwSize nelems;
  mwSize numcells;
  const mwSize *dims;
  double *dreal;
  double *dimag;
  float *sreal;
  float *simag;

  ofstream output;

  /* Protobuf object types */
  DataStore ds;
  DataGroup *dg;
  DataValue *dv;
  
  if(nlhs != 0){
    mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore","Mex function does not return any values.");
  }

  if(nrhs != 3 || !mxIsChar(prhs[0]) || !mxIsChar(prhs[1])){
    mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore","Mex function takes a file name, precision string and a single structure of named literals.");
  }

  /* Check the type/precision requested */
  precision = mxArrayToString(prhs[1]);
  if(0 == strcmp(precision, "double")){
    prec = DataGroup::DOUBLE;
  }else if(0 == strcmp(precision, "complexdouble")){
    prec = DataGroup::COMPLEXDOUBLE;
  }else if(0 == strcmp(precision, "float")){
    prec = DataGroup::SINGLE;
  }else if(0 == strcmp(precision, "complexfloat")){
    prec = DataGroup::COMPLEXSINGLE;
  }else{
    mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore", "Invalid precision requested '%s'.\n", precision);
  }
  mxFree(precision);

  matstruct = prhs[2];
  ndims = mxGetNumberOfDimensions(matstruct);
  dims = mxGetDimensions(matstruct);

  /* Verify that the data input is a structure and does not have multiple dimensions */
  if(!mxIsStruct(matstruct) ||
     2 != ndims ||
     1 != dims[0] ||
     1 != dims[1]){
    mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore","Mex function takes file name, precision string and a single structure of named literals.");
  }

  numfields = mxGetNumberOfFields(matstruct);

  for(i=0;i<numfields;i++){
    fieldname = mxGetFieldNameByNumber(matstruct, i);

    /* Check to see if struct field is a cell array */
    matcell = mxGetFieldByNumber(matstruct, 0, i);
    if(mxIsCell(matcell)){
      ndims = mxGetNumberOfDimensions(matcell);
      dims = mxGetDimensions(matcell);
      numcells = 1;
      for(j=0;j<ndims;j++){
	numcells *= dims[j];
      }
      matfield = mxGetCell(matcell, 0);
    }
    else{
      numcells = 1;
      matfield = matcell;
    }

    /* Create a new DataGroup corresponding to the structure field */
    dg = ds.add_group();
    dg->set_name(fieldname);
    dg->set_type(prec);

    /* Add a DataValue for each cell in structure field, only one member if field is numeric */
    for(j=0;j<numcells;j++){
      /* Ensure data element is double precision numeric data if requested precision is double */
      if(!mxIsDouble(matfield) && (DataGroup::DOUBLE == prec || DataGroup::COMPLEXDOUBLE == prec)){
	mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore","Field '%s' does not contain a double precision numeric literal at position %d.  Type is '%s'.", fieldname, j, mxGetClassName(matfield));
      }
      
      /* Ensure data element is double or single precision if requested precision is single */
      if(!mxIsDouble(matfield) && !mxIsSingle(matfield) && (DataGroup::SINGLE == prec || DataGroup::COMPLEXSINGLE == prec)){
	mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore", "Field '%s' does not contain a floating point literal at position %d.  Type is '%s'.", fieldname, j, mxGetClassName(matfield));
      }

      /* Ensure data is not complex if only reals were specified */
      if(mxIsComplex(matfield) && (DataGroup::DOUBLE == prec || DataGroup::SINGLE == prec)){
	mexErrMsgIdAndTxt("Simatra:simEngine:mexDataStore", "Field '%s' contains a complex literal at position %d but only real values were requested.", fieldname, j);
      }
      
      /* Store the contents of the DataValue */
      ndims = mxGetNumberOfDimensions(matfield);
      dims = mxGetDimensions(matfield);
      nelems = 1;

      /* Get pointers to the Matlab data */
      if(mxIsDouble(matfield)){
	dreal = mxGetPr(matfield);
	dimag = mxGetPi(matfield);
	sreal = NULL;
	simag = NULL;
      }else if(mxIsSingle(matfield)){
	sreal = (float*)mxGetData(matfield);
	simag = (float*)mxGetImagData(matfield);
	dreal = NULL;
	dimag = NULL;
      }

      dv = dg->add_member();

      for(k=0;k<ndims;k++){
	nelems *= dims[k];
	dv->add_dims(dims[k]);
      }
      for(k=0;k<nelems;k++){
	/* Add data based on type/precision */
	switch(prec){
	case DataGroup::COMPLEXDOUBLE:
	  if(dimag){
	    dv->add_dimag(dimag[k]);
	  }else{
	    /* Value had no imaginary data, but complex type is requested */
	    dv->add_dimag(0.0);
	  }
	  /* no break, case falls through to next */
	case DataGroup::DOUBLE:
	  dv->add_dreal(dreal[k]);
	  break;

	  /* We allow downconversion from double to single so must check type of data */
	case DataGroup::COMPLEXSINGLE:
	  if(dimag){
	    dv->add_simag((float)dimag[k]);
	  }else if(simag){
	    dv->add_simag(simag[k]);
	  }else{
	    /* Value had no imaginary data, but complex type is requested */
	    dv->add_simag(0.0f);
	  }
	  /* no break, case falls through to next */
	case DataGroup::SINGLE:
	  if(dreal){
	    dv->add_sreal((float)dreal[k]);
	  }else{
	    dv->add_sreal(sreal[k]);
	  }
	  break;
	}
      }

      /* If structure field is a cell array, advance to next cell */
      if(numcells > 1){
	matfield = mxGetCell(matcell, j+1);
      }
    }
  }

  /* Open the file */
  filename = mxArrayToString(prhs[0]);
  output.open(filename, ios::out | ios::binary | ios::trunc);
  mxFree(filename);

  /* Write entire DataStore to file */
  ds.SerializeToOstream(&output);

  output.close();
  google::protobuf::ShutdownProtobufLibrary();
}
