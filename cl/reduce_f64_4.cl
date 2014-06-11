__kernel
void reduce_f64(__global double* buffer,
            __const int block,
            __const int length,
            __global double* result) {
  int global_index = get_global_id(0) * block;
  double accumulator = INFINITY;
  int upper_bound = (get_global_id(0) + 1) * block;
  if (upper_bound > length) upper_bound = length;
  while (global_index < upper_bound) {
    double element = buffer[global_index];
    accumulator = fun_name(accumulator, element);
    global_index++;
  }
  result[get_group_id(0)] = accumulator;
}
