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

-include_lib("erlando/include/do.hrl").

%% API
-define(PROVIDER, upgrade).
-define(DEPS, [upgrade_config]).

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
    {ok, Cwd} = file:get_cwd(),
    AllInOne = cluster_booter_state:all_in_one(State),
    ClusupName = atom_to_list(AllInOne) ++ ".clusup",
    ClusupPath = filename:join([Cwd, "releases", ClusupName]),
    do([error_m ||
           {clusup, ClusterName, UpVsn, _DownVsn, ReleaseChanges, _AppChanges, Extra} <-
               cluster_booter_file_lib:consult_clusup(ClusupPath),
           monad_error:trans_error(pre_upgrade(Extra, State), fun(Reason) -> {pre_upgrade_failed, Reason} end, error_m),
           State1 <- upgrade_changes(ClusterName, ReleaseChanges, State),
           State2 <- make_permenants(ClusterName, ReleaseChanges, State1),
           State3 = cluster_booter_state:version(State2, UpVsn),
           cluster_booter_file_lib:copy_clusfile(State3),
           monad_error:trans_error(post_upgrade(Extra, State), fun(Reason) -> {post_upgrade_failed, Reason} end, error_m),
           return(State3)
       ]).

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
    io:format("upgrade ~p from ~s to ~s ~n", [NodeName, FromVsn, Vsn]),
    case upgrade_change(NodeName, Vsn, FromVsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(ClusterName, [{add, NodeName, Vsn}|T], State) ->
    io:format("start new node ~p of ~s ~n", [NodeName, Vsn]),
    case upgrade_add(NodeName, Vsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(ClusterName, [{remove, NodeName, Vsn}|T], State) ->
    io:format("stop node ~p of ~s ~n", [NodeName, Vsn]),
    case upgrade_remove(NodeName, Vsn, State) of
        {ok, State1} ->
            upgrade_changes(ClusterName, T, State1);
        {error, Reason} ->
            {error, Reason}
    end;
upgrade_changes(_ClusterName, [], State) ->
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

pre_upgrade(Extra, State) ->
    case proplists:get_value(pre_upgrade, Extra) of
        undefined ->
            ok;
        Scripts ->
            traverable:traverse(fun(Script) -> apply_upgrade_script(Script, State) end, Scripts, list)
    end.

post_upgrade(Extra, State) ->
    case proplists:get_value(post_upgrade, Extra, []) of
        [] ->
            ok;
        Scripts ->
            traverable:traverse(fun(Script) -> apply_upgrade_script(Script, State) end, Scripts, list)
    end.

apply_upgrade_script({NodeName, {M, F, A}}, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    io:format("call to ~p ~p ~p ~p~n", [NodeName, M, F, A]),
    case rpc:call(Node, M, F, A) of
        ok ->
            ok;
        Other ->
            {error, {upgrade_failed, NodeName, M, F, A, Other}}
    end.

upgrade_change(NodeName, Vsn, _FromVsn, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    case set_unpacked(NodeName, Vsn, State) of
        {ok, _} ->
            case install_release(Node, Vsn) of
                {ok, {Vsn, OldVsn}} ->
                    io:format("~p installed new release ~s replace of ~s~n", [NodeName, Vsn, OldVsn]),
                    {ok, State};
                {ok, Vsn} ->
                    io:format("~p already installed new release ~s~n", [NodeName, Vsn]),
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

upgrade_add(_NodeName, _Vsn, State) ->
    {ok, State}.

upgrade_remove(_NodeName, _Vsn, State) ->
    {ok, State}.

set_unpacked(NodeName, Vsn, State) ->
    RootDir = cluster_booter_state:root(State),
    NodeStr = atom_to_list(NodeName),
    RelFile = filename:join([RootDir, "clients", NodeStr, "releases", Vsn, NodeStr ++ ".rel"]),
    Node = cluster_booter_state:get_node(NodeName, State),
    case rpc:call(Node, release_handler, set_unpacked, [RelFile, []]) of
        {ok, Vsn} ->
            {ok, Vsn};
        {error, {existing_release, Vsn}}  ->
            {ok, Vsn};
        {error, Reason} ->
            {error, Reason}
    end.

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
