# ParaPhrase Summer School 2014: Erlang Offloading

## Directory Structure

| Path    | Description 
| ------- |:-----------------------------:|
| ebin/   | Beam files
| src/    | Erlang source code
| c_src/  | C source code
| cl/     | OpenCL kernels and functions
| jl/     | Julia source code
| doc/    | Documentation

## Installation

```bash
make
make make_boot
```

### Testing

```bash
./start.sh
```

```erlang
cl_example_srv:process_data(cl_example:gen_rands(1024)).
```