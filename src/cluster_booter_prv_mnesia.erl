%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_mnesia).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, mnesia).
-define(DEPS, [node_status]).

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
    UnstartedNodes = cluster_booter_state:unstarted_nodes(State),
    UndefinedNodes = cluster_booter_state:unstarted_nodes(State),
    ApplicationSt = cluster_booter_state:application_st(State),
    case {UnstartedNodes, UndefinedNodes} of
        {[], []} ->
            SchemaModule = cluster_booter_state:mnesia_schema(State),
            MnesiaNodeMap = cluster_booter_state:mnesia_nodes(State),
            NodeMap = cluster_booter_state:node_map(State),
            Result = 
                maps:fold(
                  fun(NodeName, MnesiaNodes, Acc) ->
                          case cluster_booter_mnesia:initialize(MnesiaNodes, NodeMap, SchemaModule) of
                              ok ->
                                  Acc;
                              {error, Reason} ->
                                  maps:put(NodeName, Reason, Acc)
                          end
                  end, maps:new(), MnesiaNodeMap),
            case maps:size(Result) of
                0 ->
                    {ok, State};
                _ ->
                    {error, Result}
            end;
        _ ->
            {error, {nodes_unready, UnstartedNodes ++ UndefinedNodes}}
    end.

format_error({nodes_unready, Nodes}) when is_list(Nodes) ->
    NodesStr = string:join(lists:map(fun(Node) -> atom_to_list(Node) end, Nodes), ","),
    io_lib:format("Nodes not ready ~s", [NodesStr]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_tables(MnesiaNodes, NodeMap, SchemaModule) ->
    cluster_booter_mnesia:initialize(MnesiaNodes, NodeMap, SchemaModule).