== ParaPhrase Summer School 2014: Erlang Offloading

Directory Structure

ebin/   -- Beam files
src/    -- Erlang source code
c_src/  -- C source code
cl/     -- OpenCL kernels and functions
jl/     -- Julia source code
doc/    -- Documentation

Installation

$ make
$ make make_boot

Testing

$ ./start.sh

cl_example:init().
cl_example_srv:process_data(cl_example:gen_rands(1024)).