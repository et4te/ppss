%%------------------------------------------------------------------------------
%% Author: et4te <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(cl_example).

-on_load(init/0).

%%-export([init/0]).
-export([gen_rands/1]).
-export([square/1, sum/1, min/1, max/1]).

-export([test/0]).

%%------------------------------------------------------------------------------
init() ->
  ok = jl_nif:load_file("jl/ocl_skeleton.jl"),
  ok = jl_nif:load_file("jl/ocl_pattern.jl").

%%------------------------------------------------------------------------------
%% Calls to Map / Reduce kernels
%%------------------------------------------------------------------------------
square(Data) ->
  jl_nif:run(1024, 16, "OclMap", "cl/map_f64.cl", "cl/fun/square_f64.cl", 
	     "square_f64", Data).

sum(Data) ->
  jl_nif:run(1024, 16, "OclReduce", "cl/reduce_f64_1.cl", "cl/fun/sum_f64.cl", 
	     "sum_f64", Data).

min(Data) ->
  jl_nif:run(1024, 16, "OclReduce", "cl/reduce_f64_2.cl", "cl/fun/min_f64.cl", 
	     "min_f64", Data).

max(Data) ->
  jl_nif:run(1024, 16, "OclReduce", "cl/reduce_f64_3.cl", "cl/fun/max_f64.cl", 
	     "max_f64", Data).

%%------------------------------------------------------------------------------
%% Utility
%%------------------------------------------------------------------------------
gen_rands(N) ->
  gen_rands(N, []).

gen_rands(0,Acc) ->
  Acc;
gen_rands(N,Acc) ->
  gen_rands(N-1,[random:uniform()|Acc]).


test() ->  
  init(),
  Xs = gen_rands(1024),
  cl_example:square(Xs),
  cl_example:sum(Xs),
  cl_example:min(Xs),
  cl_example:max(Xs).
  



