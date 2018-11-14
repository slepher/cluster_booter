%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_stop_nodes).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, stop_nodes).
-define(DEPS, []).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "stop erlang nodes."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    Nodes = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    case cluster_booter_prv_node_status:do(State) of
        {ok, NState} ->
            cluster_booter_state:fold_host_nodes(
              fun(Host, Release, NodeName, ok) ->
                      case cluster_booter_state:node_started(NodeName, NState) of
                          false ->
                              io:format("node ~p is already stoppped at ~s~n", [NodeName, Host]);
                          true ->
                              Node = maps:get(NodeName, NodeMap),
                              case cluster_booter:rpc_call(Node, init, stop, []) of
                                  ok ->
                                      io:format("node ~p is stopped at ~s~n", [Release, Host]);
                                  {error, Reason} ->
                                      io:format("node ~p is stop failed ~p at ~s~n", [Release, Reason, Host])
                              end
                      end
              end, ok, NState),
            cluster_booter_node:wait(Nodes, NodeMap, 10000, stopped),
            cluster_booter_prv_node_status:do(NState);
        {error, Reason} ->
            {error, Reason}
    end.

format_error(_Error) ->
    ok.

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

