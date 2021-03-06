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
    Status = cluster_booter_node:check(Nodes, NodeMap),
    cluster_booter_state:fold_host_nodes(
      fun(Host, Release, NodeName, ok) ->
              case cluster_booter_node:started(NodeName, Status) of
                  false ->
                      ok;
                  true ->
                      Node = maps:get(NodeName, NodeMap),
                      case cluster_booter:rpc_call(Node, init, stop, []) of
                          ok ->
                              io:format("node ~p is stopped at ~s~n", [Release, Host]);
                          {error, Reason} ->
                              io:format("node ~p is stop failed ~p at ~s~n", [Release, Reason, Host])
                      end
              end
      end, ok, State),
    case cluster_booter_node:wait(Status, 10000, stopped) of
        {ok, NStatus} ->
            NState = cluster_booter_state:node_status(State, NStatus),
            {ok, NState};
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

