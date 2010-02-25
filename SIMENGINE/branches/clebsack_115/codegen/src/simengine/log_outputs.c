// Separates data from the output buffer into the respective files for each output

#define BYTE(val,n) ((val>>(n<<3))&0xff)

int log_outputs(output_buffer *ob, char *outputs_dirname, unsigned int modelid_offset, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  unsigned int ndata = ob->count[modelid];
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN));

  FILE *output_files[seint.num_outputs];

  // Only do any work if there is data in the buffer, should greatly speed up models with conditional outputs
  if(ndata){
    char model_dirname[PATH_MAX];
    unsigned int full_modelid = modelid+modelid_offset;
    sprintf(model_dirname, "%s", outputs_dirname);
    int i;
    for(i=2;i>=0;i--){
      sprintf((model_dirname + strlen(model_dirname)), "/%02x", BYTE(full_modelid, i));
    }

    // Open all output files for modelid
    for(outputid=0;outputid<seint.num_outputs;outputid++){
      char output_filename[PATH_MAX];

      sprintf(output_filename, "%s/%s", model_dirname, seint.output_names[outputid]);
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
