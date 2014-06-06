%%------------------------------------------------------------------------------
%% Author: et4te <edward.tate@erlang-solutions.com>
%%------------------------------------------------------------------------------
-module(cl_example_srv).

-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% gen_server Callbacks
%%------------------------------------------------------------------------------
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%------------------------------------------------------------------------------
%% External Exports
%%------------------------------------------------------------------------------
-export([start_link/0, stop/0]).
-export([process_data/1]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%==============================================================================
%% External
%%==============================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

process_data(Data) ->
  gen_server:call(?SERVER, {process_data, Data}).

%%==============================================================================
%% Server Functions
%%==============================================================================
init([]) ->
  {ok, []}.

handle_call({process_data, Data}, From, State) ->
  R = cl_example:sum(Data),
  {reply, R, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

terminate(_,_) ->
  ok.

code_change(_,_,_) ->
  not_implemented.

handle_info(_, _) ->
  not_implemented.
