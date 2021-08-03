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

-include_lib("erlando/include/do.hrl").

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
    {ok, Cwd} = file:get_cwd(),
    AllInOne = cluster_booter_state:all_in_one(State),
    ClusBasename = atom_to_list(AllInOne) ++ ".clusup",
    ClusupPath = filename:join([Cwd, "releases", ClusBasename]),
    do([error_m ||
           {clusup, ClusterName, _UpVsn, DownVsn, ReleaseChanges, _AppChanges, Extra} <-
               cluster_booter_file_lib:consult_clusup(ClusupPath),
           monad_error:trans_error(pre_downgrade(Extra, State), fun(Reason) -> {pre_upgrade_failed, Reason} end, error_m),
           State1 <- downgrade_changes(ClusterName, ReleaseChanges, State),
           monad_error:trans_error(post_downgrade(Extra, State), fun(Reason) -> {post_upgrade_failed, Reason} end, error_m),
           State2 = cluster_booter_state:version(State1, DownVsn),
           cluster_booter_file_lib:copy_clusfile(State2),
           return(State2)
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

downgrade_change(NodeName, Vsn, FromVsn, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    case install_release(Node, FromVsn) of
        {ok, {FromVsn, OldVsn}} ->
            case change_vsn_and_clear(Node, Vsn, FromVsn) of
                ok ->
                    io:format("~p installed old release ~s replace of ~s~n", [NodeName, OldVsn, Vsn]),
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, FromVsn} ->
            case change_vsn_and_clear(Node, Vsn, FromVsn) of
                ok ->
                    io:format("~p already downgraded to old release ~s~n", [NodeName, FromVsn]),
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

pre_downgrade(Extra, State) ->
    case proplists:get_value(pre_downgrade, Extra) of
        undefined ->
            ok;
        Scripts ->
            traversable:traverse(fun(Script) -> apply_downgrade_script(Script, State) end, Scripts, list)
    end.

post_downgrade(Extra, State) ->
    case proplists:get_value(post_downgrade, Extra, []) of
        [] ->
            ok;
        Scripts ->
            traversable:traverse(fun(Script) -> apply_downgrade_script(Script, State) end, Scripts, list)
    end.

apply_downgrade_script({NodeName, {M, F, A}}, State) ->
    Node = cluster_booter_state:get_node(NodeName, State),
    io:format("call to ~p ~p ~p ~p~n", [NodeName, M, F, A]),
    case rpc:call(Node, M, F, A) of
        ok ->
            ok;
        Other ->
            {error, {upgrade_failed, NodeName, M, F, A, Other}}
    end.

set_removed(Node, Vsn, Releases) ->
    case maps:find(Vsn, Releases) of
        {ok, _} ->
            set_removed(Node, Vsn);
        error ->
            ok
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
           Releases <- current_releaess(Node),
           make_permenant(Node, FromVsn),
           set_removed(Node, Vsn, Releases)
       ]).

current_releaess(Node) ->
    case rpc:call(Node, release_handler, which_releases, []) of
        Releases when is_list(Releases) ->
            Statues = 
                lists:foldl(
                  fun({_Name, Vsn, _Libs, Status}, Acc) ->
                          maps:put(Vsn, Status, Acc)
                  end, #{}, Releases),
            {ok, Statues};
        {badrpc, Reason} ->
            {error, Reason}
    end.
