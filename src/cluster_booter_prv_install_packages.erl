%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_install_packages).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, install).
-define(DEPS, [packages, installed]).

-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "extract erlang application to target host"}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    Root = cluster_booter_state:root(State),
    MnesiaDir = cluster_booter_state:mnesia_dir(State),
    LogDir = cluster_booter_state:log_dir(State),
    CurrentHost = cluster_booter_state:current_host(State),
    HostInstalledPackages = cluster_booter_state:installed_packages(State),
    Packages = cluster_booter_state:packages(State),
    Hosts = cluster_booter_state:hosts(State),
    NodeReleaseMap = cluster_booter_state:node_release_map(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    Fun = fun(Host, Node, Release, Version) ->
                  Opts = [{host, Host}, {current_host, CurrentHost}],
                  mkdir(filename:join(Root, Node), Opts),
                  Args = [{release, Release}, {version, Version},
                          {node_name, Node}, {base_dir, Root}, {packages_path, PackagesPath}],
                  Cmd = cluster_booter_cmd:cmd(extract, Args, Opts),
                  io:format("cmd is ~s~n", [Cmd]),
                  os:cmd(Cmd)
          end,
    maps:fold(
      fun(Host, Nodes, ok) ->
              Opts = [{host, Host}, {current_host, CurrentHost}],
              mkdir(filename:join(Root, MnesiaDir), Opts),
              mkdir(filename:join(Root, LogDir), Opts),
              InstalledPackages = maps:get(Host, HostInstalledPackages, maps:new()),
              lists:foreach(
                fun(Node) ->
                        Installed = maps:get(Node, InstalledPackages, false),
                        case Installed of
                            false ->
                                case maps:find(Node, NodeReleaseMap) of
                                    {ok, Release} ->
                                        case get_latest_version(Release, Packages) of
                                            {ok, Version} ->
                                                CmdResult = Fun(Host, Node, Release, Version),
                                                io:format("install ~p success ~s~n", [Node, CmdResult]);
                                            {error, Reason} ->
                                                io:format("~p of ~p~n", [Reason, Release])
                                        end;
                                    error ->
                                        io:format("no_release of ~p~n", [Node])
                                end;
                            true ->
                                io:format("~p is already installed~n", [Node])
                        end
                end, Nodes)
      end, ok, Hosts),
    cluster_booter_prv_installed_packages:do(State).

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%===================================================================
%%% API
%%%===================================================================
mkdir(Path, Opts) ->
    DirArgs = [{dir, Path}],
    DirCmd = cluster_booter_cmd:cmd(mkdir, DirArgs, Opts),
    os:cmd(DirCmd).

get_latest_version(Release, Packages) ->
    case maps:find(Release, Packages) of
        {ok, VersionsMap} ->
            Versions = maps:keys(VersionsMap),
            Version = lists:max(Versions),
            {ok, Version};
        error ->
            {error, no_usabe_packages}
    end.
            
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
