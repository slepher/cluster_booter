%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_downgrade).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, downgrade).
-define(DEPS, []).

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
            case downgrade_changes(ClusterName, Changes, State) of
                {ok, State1} ->
                    make_permenants(ClusterName, Changes, State1);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, [{clusup, ClusterName, Changes, _Extra}]} ->
            case downgrade_changes(ClusterName, Changes, State) of
                {ok, State1} ->
                    make_permenants(ClusterName, Changes, State1);
                {error, Reason} ->
                    {error, Reason}
            end;
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
downgrade_changes(ClusterName, [{change, NodeName, Vsn, FromVsn}|T], State) ->
    io:format("downgrade ~p from ~s to ~s ~n", [NodeName, Vsn, FromVsn]),
    case downgrade_change(NodeName, Vsn, FromVsn, State) of
        {ok, State1} ->
            downgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
downgrade_changes(ClusterName, [{add, NodeName, Vsn}|T], State) ->
    io:format("stop new node ~p of ~s ~n", [NodeName, Vsn]),
    case downgrade_remove(NodeName, Vsn, State) of
        {ok, State1} ->
            downgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
downgrade_changes(ClusterName, [{remove, NodeName, Vsn}|T], State) ->
    io:format("start new node ~p of ~s ~n", [NodeName, Vsn]),
    case downgrade_add(NodeName, Vsn, State) of
        {ok, State1} ->
            downgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
downgrade_changes(_ClusterName, [], State) ->
    {ok, State}.

make_permenants(ClusterName, [{change, NodeName, Vsn, _}|T], State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    case rpc:call(Node, release_handler, make_permanent, [Vsn]) of
        ok ->
            io:format("make ~p new release ~s permanent~n", [NodeName, Vsn]),
            make_permenants(ClusterName, T, State);
        {error, Reason} ->
            {error, Reason}
    end;
make_permenants(ClusterName, [_Other|T], State) ->
    make_permenants(ClusterName, T, State);
make_permenants(_ClusterName, [], State) ->
    {ok, State}.

downgrade_change(NodeName, Vsn, FromVsn, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    case install_release(Node, FromVsn) of
        {ok, {Vsn, OldVsn}} ->
            case change_vsn_and_clear(Node, Vsn, FromVsn) of
                ok ->
                    io:format("~p installed old release ~s replace of ~s~n", [NodeName, OldVsn, Vsn]),
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Vsn} ->
            case change_vsn_and_clear(Node, Vsn, FromVsn) of
                ok ->
                    io:format("~p already installed new release ~s~n", [NodeName, Vsn]),
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

downgrade_add(_NodeName, _Vsn, State) ->
    {ok, State}.

downgrade_remove(_NodeName, _Vsn, State) ->
    {ok, State}.

install_release(Node, Vsn) ->
    case rpc:call(Node, release_handler, install_release, [Vsn]) of
        {ok, OldVsn, _} ->
            {ok, {Vsn, OldVsn}};
        {error, {already_installed, Vsn}} ->
            {ok, Vsn};
        {error, {no_matching_relup, Vsn, Vsn}} ->
            {ok, Vsn};
        {error, Reason} ->
            {error, Reason}
    end.

make_permenant(Node, Vsn) ->
    case rpc:call(Node, release_handler, make_permanent, [Vsn]) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

set_removed(Node, Vsn) ->
    case rpc:call(Node, release_handler, set_removed, [Vsn]) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


change_vsn_and_clear(Node, Vsn, FromVsn) ->
    do([error_m ||
           make_permenant(Node, FromVsn),
           set_removed(Node, Vsn)
       ]).
