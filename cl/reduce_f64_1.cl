__kernel
void reduce_f64(
 __global double* input,
 __local double* scratch,
 __const int length,
 __global double* output
) 
{
  int gidx = get_global_id(0);
  int lidx = get_local_id(0);

  if (gidx < length) {
    scratch[lidx] = input[gidx];
  } else {
    scratch[lidx] = INFINITY;
  }

  barrier(CLK_LOCAL_MEM_FENCE);
  for (int offset = 1; offset < get_local_size(0); offset <<= 1) {
    int mask = (offset << 1) - 1;
    if ((lidx & mask) == 0) {
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
