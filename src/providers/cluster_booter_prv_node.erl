%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_node).
-behaviour(provider).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, nodes_started).
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
    UNodeNames = 
        lists:filter(
          fun(NodeName) ->
                  not maps:is_key(NodeName, NodeMap)
          end, NodeNames),
    case UNodeNames of
        [] ->
            Nodes = maps:values(maps:with(NodeNames, NodeMap)),
            case cluster_booter_node:nodes_stopped(Nodes) of
                [] ->
                    {ok, State};
                Unstarted ->
                    {error, {unstarted_nodes, Unstarted}}
            end;
        _ ->
            {error, {unconfigured_nodes, UNodeNames}}
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
