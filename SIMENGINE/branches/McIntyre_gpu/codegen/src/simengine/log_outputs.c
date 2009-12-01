/* Transmutes the internal data buffer into the structured output
 * which may be retured to the client.
 */
int log_outputs(output_buffer *ob, simengine_output *outputs, unsigned int modelid) {
  unsigned int outputid, nquantities, dataid, quantityid;
  simengine_output *output;
  double *odata;
	     
  unsigned int ndata = ob->count[modelid];
  output_buffer_data *buf = (output_buffer_data *)(ob->buffer + (modelid * BUFFER_LEN));
	     
  for (dataid = 0; dataid < ndata; ++dataid) {
    outputid = buf->outputid;
    assert(seint.num_outputs > outputid);

    nquantities = buf->num_quantities;
    assert(seint.output_num_quantities[outputid] == nquantities);

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
		 
    // Copies each element individually for implicit type conversion from CDATAFORMAT to double.
    for (quantityid = 0; quantityid < nquantities; ++quantityid) {
      odata[quantityid] = buf->quantities[quantityid];
    }
		 
    buf = (output_buffer_data *)(buf->quantities + buf->num_quantities);
    ++output->num_samples;
  }
	     
  return 0;
}
