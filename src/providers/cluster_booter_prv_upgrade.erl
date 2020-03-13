%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_upgrade).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, upgrade).
-define(DEPS, [install_upgrade]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "start erlang applications on already started nodes."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    PackagesPath = cluster_booter_state:packages_path(State),
    ClusupPath = filename:join([PackagesPath, "clusup"]),
    case file:consult(ClusupPath) of
        {ok,[{clusup, ClusterName, Changes}]} ->
            upgrade_changes(ClusterName, Changes, State);
        {ok, [{clusup, ClusterName, Changes, _Extra}]} ->
            upgrade_changes(ClusterName, Changes, State);
        {error, Reason} ->
            {error, Reason}
    end.

format_error({application_not_started, Result}) ->
    io_lib:format("application start failed ~p", [Result]).

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
upgrade_changes(ClusterName, [{change, NodeName, Vsn, FromVsn}|T], State) ->
    case upgrade_change(NodeName, Vsn, FromVsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(ClusterName, [{add, NodeName, Vsn}|T], State) ->
    case upgrade_add(NodeName, Vsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(ClusterName, [{remove, NodeName, Vsn}|T], State) ->
    case upgrade_remove(NodeName, Vsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(_ClusterName, [], State) ->
    {ok, State}.

upgrade_change(NodeName, Vsn, _FromVsn, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    case rpc:call(Node, release_handler, install_release, [Vsn]) of
        {ok, OldVsn, _} ->
            io:format("~p installed new release ~s replace of ~s", [NodeName, Vsn, OldVsn]),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

upgrade_add(_NodeName, _Vsn, State) ->
    {ok, State}.

upgrade_remove(_NodeName, _Vsn, State) ->
    {ok, State}.
