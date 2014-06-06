%%------------------------------------------------------------------------------
%% Author: et4te <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(jl_nif).

-on_load(init/0).

-export([init/0]).

-export([load_file/1, run/7]).

init() ->
  erlang:load_nif("./priv/jl_nif", 0).

load_file(_Filename) ->
  {error, "NIF library not loaded"}.

run(GlobalSize, LocalSize, KernelType, KernelSrc, FunSrc, FunName, Input) ->
  {error, "NIF library not loaded"}.




  
