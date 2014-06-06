__kernel
void reduce_f64(
 __global double* input,
 __local double* scratch,
 __const int length,
 __global double* output
) 
{
  int gidx = get_global_id(0);
  double accumulator = INFINITY;
  while (gidx < length) {
    double element = input[gidx];
    accumulator = fun_name(accumulator, element);
    gidx += get_global_size(0);
  }	

  int lidx = get_local_id(0);
  scratch[lidx] = accumulator;
  barrier(CLK_LOCAL_MEM_FENCE);
  for (int offset = get_local_size(0) / 2; offset > 0; offset >>= 1) {
    if (lidx < offset) {
      double other = scratch[lidx + offset];
      double mine  = scratch[lidx];
      scratch[lidx] = fun_name(mine, other);
    }
    barrier(CLK_LOCAL_MEM_FENCE);
  }

  if (lidx == 0) {
    output[get_group_id(0)] = scratch[0];
  }
}
