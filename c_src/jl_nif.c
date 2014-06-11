//------------------------------------------------------------------------------
// Author: et4te <edward.tate@erlang-solutions.com>
//------------------------------------------------------------------------------
#include <julia.h>
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include "erl_nif.h"

//==============================================================================
// Utility
//==============================================================================

static int
custom_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf)
{
  ERL_NIF_TERM cell, head, tail;
  int len, val;

  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_int(env, head, &val)) 
      {
	return 0;
      }
    *buf = (char) val;
    buf++;
    list = tail;
  }
  *buf = '\0';

  return 1;
}

static int
custom_get_float64_array(ErlNifEnv* env, ERL_NIF_TERM list, double* buf)
{
  ERL_NIF_TERM cell, head, tail;
  int len;
  double val;

  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_double(env, head, &val)) 
      {
	return 0;
      }
    *buf = (double) val;
    buf++;
    list = tail;
  }
  *buf = '\0';

  return 1;
}

//==============================================================================
// Julia API
//==============================================================================

static ERL_NIF_TERM
load_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  JL_SET_STACK_BASE;

  int len;

  if (argc != 1)
    {
      return enif_make_badarg(env);
    }

  if (!enif_get_list_length(env, argv[0], &len)) 
    {
      return enif_make_badarg(env);
    }

  char* filename = (char*) malloc(sizeof(char) * len);
  
  if (!custom_get_string(env, argv[0], filename)) 
    {
      return enif_make_badarg(env);
    }

  jl_value_t* val = jl_load(filename);

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  JL_SET_STACK_BASE;
  
  int global_size, local_size;
  int kernel_type_len, kernel_path_len;
  int fun_path_len, fun_name_len;
  int input_len, output_len;

  if (argc != 8) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[0], &global_size)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &local_size)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[2], &kernel_type_len)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_list_length(env, argv[3], &kernel_path_len)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_list_length(env, argv[4], &fun_path_len)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_list_length(env, argv[5], &fun_name_len)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_list_length(env, argv[6], &input_len)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[7], &output_len)) {
    return enif_make_badarg(env);
  }

  char* kernel_type = (char*) malloc(sizeof(char) * kernel_type_len);
  if (!custom_get_string(env, argv[2], kernel_type)) {
    return enif_make_badarg(env);
  }
  char* kernel_path = (char*) malloc(sizeof(char) * kernel_path_len);
  if (!custom_get_string(env, argv[3], kernel_path)) {
    return enif_make_badarg(env);
  }
  char* fun_path = (char*) malloc(sizeof(char) * fun_path_len);
  if (!custom_get_string(env, argv[4], fun_path)) {
    return enif_make_badarg(env);
  }
  char* fun_name = (char*) malloc(sizeof(char) * fun_name_len);
  if (!custom_get_string(env, argv[5], fun_name)) {
    return enif_make_badarg(env);
  }

  double* input = (double*) malloc(sizeof(double) * input_len);
  if (!custom_get_float64_array(env, argv[6], input)) {
    return enif_make_badarg(env);
  }

  jl_value_t* ocl_sk_mod = jl_eval_string("OclSkeleton");
  jl_function_t* ocl_run =
    jl_get_function((jl_module_t*) ocl_sk_mod, "run");

  jl_function_t* ocl_kernel_type =
    jl_get_function((jl_module_t*) ocl_sk_mod, kernel_type);
  
  jl_value_t* ocl_global_size = jl_box_uint32(global_size);
  jl_value_t* ocl_local_size = jl_box_uint32(local_size);

  jl_value_t* ocl_kernel_path = jl_pchar_to_string(kernel_path, kernel_path_len);
  jl_value_t* ocl_fun_path = jl_pchar_to_string(fun_path, fun_path_len);
  jl_value_t* ocl_fun_name = jl_pchar_to_string(fun_name, fun_name_len);

  free(kernel_path);
  free(fun_path);
  free(fun_name);

  const int n_kernel_args = 5;
  jl_value_t** ocl_kernel_args = 
    (jl_value_t**) malloc(sizeof(jl_value_t*) * n_kernel_args);

  const int n_run_args = 2;
  jl_value_t** ocl_run_args =
    (jl_value_t**) malloc(sizeof(jl_value_t*) * n_run_args);

  jl_value_t* ocl_array_type = jl_apply_array_type(jl_float64_type, 1);

  jl_array_t* ocl_input = jl_ptr_to_array_1d(ocl_array_type, input, input_len, 1);
  
  ocl_kernel_args[0] = ocl_global_size;
  ocl_kernel_args[1] = ocl_local_size;
  ocl_kernel_args[2] = ocl_kernel_path;
  ocl_kernel_args[3] = ocl_fun_path;
  ocl_kernel_args[4] = ocl_fun_name;

  jl_value_t* res1 = jl_call(ocl_kernel_type, ocl_kernel_args, n_kernel_args);

  ocl_run_args[0] = res1;
  ocl_run_args[1] = (jl_value_t*) ocl_input;

  jl_value_t* res2 = jl_call(ocl_run, ocl_run_args, n_run_args);

  // FIXME: output_len varies
  double* data = (double*) jl_array_data(res2);

  ERL_NIF_TERM* ocl_output = (ERL_NIF_TERM*) malloc(sizeof(ERL_NIF_TERM) * output_len);

  int i;
  for (i = 0; i < output_len; i++) {
    ocl_output[i] = enif_make_double(env, data[i]);
  }
  free(input);

  return enif_make_list_from_array(env, ocl_output, output_len);
}

//==============================================================================
// NIF API
//==============================================================================

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  void* handle = dlopen("libjulia.so", (RTLD_NOW | RTLD_GLOBAL));

  if (!handle) {
    fprintf(stderr, "%s\n", dlerror());
    exit(EXIT_FAILURE);
  }
  dlerror();
  
  jl_init(NULL);

  JL_SET_STACK_BASE;

  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
  return;
}

static ErlNifFunc nif_funcs[] = {
  {"load_file",1,load_file},
  {"run",8,run}
};

ERL_NIF_INIT(jl_nif, nif_funcs, &load, NULL, &upgrade, &unload);


