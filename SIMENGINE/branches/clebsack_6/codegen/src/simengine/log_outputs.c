
int log_outputs_streaming(unsigned int modelid_offset, unsigned int modelid) {
  global_ob[global_ob_idx[modelid]].modelid_offset[modelid] = modelid_offset;
  // Tell consumer buffer is ready
  global_ob[global_ob_idx[modelid]].available[modelid] = 1;

  // Advance to next output buffer
  global_ob_idx[modelid] = (global_ob_idx[modelid] + 1) % global_ob_count;

  // Wait for buffer to be empty before writing any new data to ob
  while(global_ob[global_ob_idx[modelid]].available[modelid]){
    usleep(10);
  }

  return 0;
}

// Separates data from the output buffer into the respective files for each output
int log_outputs_raw_files(const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
  output_buffer *ob = global_ob;
  unsigned int outputid, nquantities, dataid, quantityid;
  unsigned int ndata = ob->count[modelid];
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN));

  FILE *output_files[seint.num_outputs];

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

int log_outputs(const char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
#if NUM_OUTPUTS == 0
  return 0;
#endif

  /* Redirect to the appropriate output data handler */
  if(simex_output_files)
    return log_outputs_raw_files(outputs_dirname, modelid_offset, modelid);
  else
    return log_outputs_streaming(modelid_offset, modelid);

}
