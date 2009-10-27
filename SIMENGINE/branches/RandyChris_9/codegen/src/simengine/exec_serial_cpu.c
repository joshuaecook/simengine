#ifndef TARGET_GPU
// Run all models serially on a single cpu core
int exec_serial_cpu(solver_props *props){
  int ret = SUCCESS;
  unsigned int modelid;

  for(modelid=0;modelid<NUM_MODELS;modelid++){
    ret = exec_cpu(props, modelid);
    if(ret != SUCCESS){
      return ret;
    }
  }
  return ret;
}
#endif // ndef TARGET_GPU
