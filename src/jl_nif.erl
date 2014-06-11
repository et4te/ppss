%%------------------------------------------------------------------------------
%% Author: et4te <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(jl_nif).

-on_load(init/0).

-export([init/0]).

-export([load_file/1, run/8]).

init() ->
  erlang:load_nif("./priv/jl_nif", 0).

load_file(_Filename) ->
  {error, "NIF library not loaded"}.

run(_GSize, _LSize, _KType, _KSrc, _FSrc, _FName, _In, _OutLen) ->
  {error, "NIF library not loaded"}.




  
