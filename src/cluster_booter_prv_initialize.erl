%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_initialize).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, initialize).
-define(DEPS, [install, config, start_node, mnesia, start_apps]).

-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "initialize nodes and start all applications."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

                        
