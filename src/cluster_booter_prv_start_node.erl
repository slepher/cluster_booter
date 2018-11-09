%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_start_node).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, start_node).
-define(DEPS, [installed, node_status]).

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
    BaseDir = cluster_booter_state:root(State),
    cluster_booter_state:fold_host_nodes(
      fun(Host, Release, NodeName, ok) ->
              case cluster_booter_state:installed(Host, Release, State) of
                  true ->
                      case cluster_booter_state:node_started(NodeName, State) of
                          false ->
                              CmdOpt = cluster_booter_state:cmd_opt(Host, State),
                              CmdArg = [{node_name, NodeName}, {release_name, Release}, {base_dir, BaseDir}],
                              Cmd = cluster_booter_cmd:cmd(start_boot, CmdArg, CmdOpt),
                              os:cmd(Cmd),
                              io:format("start ~p at ~s~n", [Release, Host]);
                          true ->
                              io:format("node ~p is already started at ~s~n", [Release, Host])
                      end;
                  false ->
                      io:format("release ~p is not installed at ~s~n", [Release, Host])
              end
      end, ok, State),
    timer:sleep(1000),
    io:format("check not start status~n"),
    cluster_booter_prv_node_status:do(State).

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
