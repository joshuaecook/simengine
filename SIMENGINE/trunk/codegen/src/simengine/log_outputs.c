// Separates data from the output buffer into the respective files for each output

void check_keep_running(){
  if(getppid() == 1)
    ERROR(Simatra::Simex::Simulation, "Parent process terminated.  Simulation will not continue to run when orphaned.");
}

int log_outputs(output_buffer *ob, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
  /* Redirect to the appropriate output data handler */
  if(simex_output_files)
    return log_outputs_raw_files(ob, outputs_dirname, modelid_offset, modelid);
  else
    return log_outputs_streaming(ob, outputs_dirname, modelid_offset, modelid);
}

int log_outputs_streaming(output_buffer *ob, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
  check_keep_running();

  if(modelid == 0){
    ob->modelid_offset = modelid_offset;
  }
  // Tell consumer buffer is ready
  ob->empty[modelid] = 0;
  // Wait for buffer to be empty before writing any new data to ob
  while(!ob->empty[modelid]){
    //usleep(1000);
  }  
}

int log_outputs_raw_files(output_buffer *ob, const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  unsigned int ndata = ob->count[modelid];
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN));

  FILE *output_files[seint.num_outputs];

  check_keep_running();

  // Only do any work if there is data in the buffer, should greatly speed up models with conditional outputs
  if(ndata){
    char model_dirname[PATH_MAX];
    unsigned int full_modelid = modelid+modelid_offset;

    modelid_dirname(outputs_dirname, model_dirname, full_modelid);

    // Open all output files for modelid
    for(outputid=0;outputid<seint.num_outputs;outputid++){
      char output_filename[PATH_MAX];

      sprintf(output_filename, "%s/outputs/%s", model_dirname, seint.output_names[outputid]);
      output_files[outputid] = fopen(output_filename, "a");
      if(NULL == output_files[outputid]){
	ERROR(Simatra::Simex::log_outputs, "could not open file '%s'\n", output_filename);
      }
    }
	     
    for (dataid = 0; dataid < ndata; ++dataid) {
      outputid = buf->outputid;
      assert(seint.num_outputs > outputid);

      nquantities = buf->num_quantities;
      assert(seint.output_num_quantities[outputid] == nquantities);

      // TODO an error code for invalid data?
      if (outputid > seint.num_outputs) { return 1; }
      if (seint.output_num_quantities[outputid] != nquantities) { return 1; }
		 
      // Copies each element individually for implicit type conversion from CDATAFORMAT to double.
      for (quantityid = 0; quantityid < nquantities; ++quantityid) {
	// TODO : Buffer binary data and fwrite in larger blocks?
	if(binary_files){
	  double val = buf->quantities[quantityid];
	  fwrite(&val, sizeof(double), 1, output_files[outputid]);
	}
	else{
	  fprintf(output_files[outputid], "%s%-.16e", ((quantityid == 0) ? "" : "\t"), buf->quantities[quantityid]);
	}
      }
      if(!binary_files){
	fprintf(output_files[outputid], "\n");
      }
		 
      buf = (output_buffer_data *)(buf->quantities + buf->num_quantities);
    }

    // Close all output files for modelid
    for(outputid=0;outputid<seint.num_outputs;outputid++){
      fclose(output_files[outputid]);
    }
  }
	     
  return 0;
}
