### ParaPhrase Summer School 2014 (Erlang Offloading)

#### Requirements

* Julia 0.3
* Erlang OTP R17
* An OpenCL runtime

The following environment variables must be defined.

```bash
export LD_LIBRARY_PATH=/path/to/julia/src/lib/:$LD_LIBRARY_PATH
export JULIA_HOME=/path/to/julia/
export ERLANG_HOME=/path/to/erlang/lib/
```

#### Installation

```bash
# compile the code
make

# create a boot file 
make make_boot

# start an OpenCL enabled node
./start.sh
```

```erlang
%% From the prompt you can test the code works:
Xs = [random:uniform() || X <- lists:seq(1,1024)].
cl_example_srv:process_data(Xs).
```

#### Description

##### Directory Structure

| Path    | Description 
| ------- |:-------------------------------------------:|
| ebin/   | Erlang compiles beam code to this directory
| src/    | Erlang sources
| c_src/  | C sources
| cl/     | OpenCL kernels
| cl/fun/ | OpenCL functions
| jl/     | Julia sources
| doc/    | Documentation

##### Detailed Description

###### Erlang Sources (src)
###### Application (src/cl_example_app.erl)
###### Supervisor (src/cl_example_sup.erl)
###### Server (src/cl_example_srv.erl)
###### Examples (src/cl_example.erl)
###### NIF (src/jl_nif.erl)

###### C Sources (c_src)

There is only one C source file which is responsible for binding Erlang and 
Julia together via the Erlang NIF. 

###### OpenCL Kernels (cl/*.cl)
###### OpenCL Functions (cl/fun/*.cl)
###### OpenCL Skeletons (jl/ocl_skeleton.jl)
###### OpenCL Patterns (jl/ocl_pattern.jl)
