%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_node_status).
-behaviour(provider).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, node_status).
-define(DEPS, []).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    State1 = cluster_booter_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                                        {module, ?MODULE},
                                                                        {deps, ?DEPS}])),
    {ok, State1}.

do(State) ->
    NodeNames = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    NState = 
    lists:foldl(
      fun(NodeName, StateAcc) ->
              case maps:find(NodeName, NodeMap) of
                  {ok, Node} ->
                      case net_adm:ping(Node) of
                          pong ->
                              cluster_booter_state:add_started_node(StateAcc, NodeName);
                          pang ->
                              cluster_booter_state:add_unstarted_node(StateAcc, NodeName)
                      end;
                  error ->
                      cluster_booter_state:add_undefined_node(StateAcc, NodeName)
              end
      end, State, NodeNames),
    print_node_status(NState),
    UndefinedNodes = cluster_booter_state:undefined_nodes(State),
    case UndefinedNodes of
        [] ->
            {ok, NState};
        _ ->
            {error, {unconfigured_nodes, UndefinedNodes}}
    end.

format_error({unstarted_nodes, Nodes}) when is_list(Nodes) ->
    NodesStr = string:join(lists:map(fun(Node) -> atom_to_list(Node) end, Nodes), ","),
    io_lib:format("Nodes not started ~s", [NodesStr]);
format_error({unconfigured_nodes, Nodes}) when is_list(Nodes) ->
    NodesStr = string:join(lists:map(fun(Node) -> atom_to_list(Node) end, Nodes), ","),
    io_lib:format("Nodes not configured ~s", [NodesStr]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

print_node_status(State) ->
    StartedNodes = cluster_booter_state:started_nodes(State),
    UnstartedNodes = cluster_booter_state:unstarted_nodes(State),
    UndefinedNodes = cluster_booter_state:undefined_nodes(State),
    print_nodes_with_status(StartedNodes, started),
    print_nodes_with_status(UnstartedNodes, unstarted),
    print_nodes_with_status(UndefinedNodes, undefined).

print_nodes_with_status(Nodes, Status) ->
    lists:foreach(
      fun(Node) ->
              io:format("node ~p ~p.~n", [Node, Status])
      end, Nodes). 
