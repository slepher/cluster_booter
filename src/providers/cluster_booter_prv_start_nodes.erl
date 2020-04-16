%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_start_nodes).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, start_nodes).
-define(DEPS, [installed, config]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "start erlang nodes with or without application."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    io:format("all in one is ~p~n", [AllInOne]),
    BaseDir = cluster_booter_state:root(State),
    Nodes = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    Status = cluster_booter_node:check(Nodes, NodeMap),
    Releases = cluster_booter_state:releases(State),
    cluster_booter_state:fold_host_nodes(
      fun(Host, Release, NodeName, ok) ->
              Node = maps:get(NodeName, NodeMap),
              case cluster_booter_state:installed(Host, Release, State) of
                  true ->
                      case cluster_booter_node:started(NodeName, Status) of
                          false ->
                              case maps:find(Release, Releases) of
                                  {ok, ReleaseVsn} ->
                                      CmdOpt = cluster_booter_state:cmd_opt(Host, State),
                                      CmdArg = [{node_name, NodeName}, {cluster_name, AllInOne},
                                                {base_dir, BaseDir}, {vsn, ReleaseVsn}],
                                      Cmd = cluster_booter_cmd:cmd(start_boot, CmdArg, CmdOpt),
                                      io:format("cmd is ~s~n", [Cmd]),
                                      os:cmd(Cmd),
                                      cluster_booter_node:wait_node(Node, 5000),
                                      io:format("start ~p at ~s~n", [Release, Host]);
                                  error ->
                                      io:format("no release vsn of ~p", [Release])
                              end;
                          true ->
                              ok
                      end;
                  false ->
                      io:format("release ~p is not installed at ~s~n", [Release, Host])
              end
      end, ok, State),
    case cluster_booter_node:wait(Status, 10000, started) of
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
