__kernel void map_f64 (
  __global double* input,
  __global double* output
  ) {
  int gidx = get_global_id(0);

  output[gidx] = fun_name(input[gidx]);
}
