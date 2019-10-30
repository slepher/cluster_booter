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
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "show erlang node status in same cluster."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    NodeNames = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    NodeStatus = cluster_booter_node:check(NodeNames, NodeMap),
    cluster_booter_node:print(NodeStatus),
    %% UndefinedNodes = cluster_booter_node:undefined_nodes(NodeStatus),
    %% case UndefinedNodes of
    %%    [] ->
    NState = cluster_booter_state:node_status(State, NodeStatus),
    {ok, NState}.
    %%     _ ->
    %%         {error, {unconfigured_nodes, UndefinedNodes}}
    %% end.

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
