%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_exists).

-behaviour(provider).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, node_appfile_exists).
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
    {ok, State}.

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
