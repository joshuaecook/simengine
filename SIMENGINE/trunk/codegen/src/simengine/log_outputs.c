static float output_buffer_consumption[NUM_MODELS];

/* Transmutes the internal data buffer into the structured output
 * which may be retured to the client.
 */
int log_outputs(output_buffer *ob, simengine_output *outputs, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  simengine_output *output;
  double *odata;
	     
  unsigned int ndata = ob->count[modelid];
  //  void *data = &(ob->buffer[modelid * BUFFER_LEN]);
  output_buffer_data *data = (output_buffer_data*)(ob->buffer + (modelid * BUFFER_LEN));
#if defined _DEBUG
  size_t quants = 0; // total number of data quantities logged
#endif

	     
  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = data->tag;
    nquantities = data->count;
		 
    // TODO an error code for invalid data?
    if (outputid > seint.num_outputs) { return 1; }
    if (seint.output_num_quantities[outputid] != nquantities) { return 1; }
		 
    output = &outputs[AS_IDX(seint.num_outputs,semeta.num_models,outputid,modelid)];
		 
    if (output->num_samples == output->alloc) {
      output->alloc *= 2;
#pragma omp critical
      {
	output->data = (double*)se_alloc.realloc(output->data, output->num_quantities * output->alloc * sizeof(double));
      }
      if (!output->data)
	{ return 1; }
    }
		 
    odata = &output->data[AS_IDX(nquantities, output->num_samples, 0, output->num_samples)];
		 
    for (quantityid = 0; quantityid < nquantities; ++quantityid) {
      odata[quantityid] = data->payload[quantityid];
    }
#if defined _DEBUG
    quants += nquantities;
#endif
		 
    data = (output_buffer_data*)(data->payload + nquantities);
    ++output->num_samples;
  }

#if defined _DEBUG
  size_t bytes = (quants * sizeof(CDATAFORMAT)) + (2 * ndata * sizeof(unsigned int));
  output_buffer_consumption[modelid] = (1.0f * bytes) / (BUFFER_LEN * sizeof(CDATAFORMAT));
#endif
		 
	     
  return 0;
}
