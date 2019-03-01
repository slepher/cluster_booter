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
    AllInOne = cluster_booter_state:all_in_one(State),
    do_packages(AllInOne, State).

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
do_packages(AllInOne, State) ->
    Root = cluster_booter_state:root(State),
    MnesiaDir = cluster_booter_state:mnesia_dir(State),
    LogDir = cluster_booter_state:log_dir(State),
    PipeDir = cluster_booter_state:pipe_dir(State),
    CurrentHost = cluster_booter_state:current_host(State),
    HostInstalledPackages = cluster_booter_state:installed_packages(State),
    Packages = cluster_booter_state:packages(State),
    Hosts = cluster_booter_state:hosts(State),
    NodeReleaseMap = cluster_booter_state:node_release_map(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    Fun = fun(Host, Node, Release, Version) ->
                  Opts = [{host, Host}, {current_host, CurrentHost}],
                  mkdir(filename:join(Root, Node), Opts),
                  Filename = filename:join(PackagesPath, atom_to_list(Release) ++ "-" ++ Version ++ ".tar.gz"),
                  TargetDirectory = filename:join(Root, Node),
                  Args = [{filename, Filename}, {target_directory, TargetDirectory}],
                  Cmd = cluster_booter_cmd:cmd(extract, Args, Opts),
                  os:cmd(Cmd)
          end,
    maps:fold(
      fun(Host, Nodes, ok) ->
              Opts = [{host, Host}, {current_host, CurrentHost}],
              mkdir(filename:join(Root, MnesiaDir), Opts),
              mkdir(filename:join(Root, LogDir), Opts),
              mkdir(filename:join(Root, PipeDir), Opts),
              InstalledPackages = maps:get(Host, HostInstalledPackages, maps:new()),
              lists:foreach(
                fun(NodeName) ->
                        mkdir(filename:join(Root, PipeDir), Opts),
                        Installed = maps:get(NodeName, InstalledPackages, false),
                        case Installed of
                            false ->
                                case maps:find(NodeName, NodeReleaseMap) of
                                    {ok, Release} ->
                                        case get_latest_version(Release, Packages) of
                                            {ok, Version} ->
                                                CmdResult = Fun(Host, NodeName, Release, Version),
                                                io:format("install ~p success ~s~n", [NodeName, CmdResult]);
                                            {error, Reason} ->
                                                io:format("~p of ~p~n", [Reason, Release])
                                        end;
                                    error ->
                                        io:format("no_release of ~p~n", [NodeName])
                                end;
                            true ->
                                io:format("~p is already installed~n", [NodeName])
                        end
                end, Nodes)
      end, ok, Hosts),
    cluster_booter_prv_installed_packages:do(State).

%% do_all_in_one(AllInOne, State) ->
%%     Hosts = cluster_booter_state:hosts(State),
%%     CurrentHost = cluster_booter_state:current_host(State),
%%     Root = cluster_booter_state:root(State),
%%     Packages = cluster_booter_state:packages(State),
%%     PackagesPath = cluster_booter_state:packages_path(State),

%%     %case get_latest_version(Release, Packages) of
%%     %    {ok, Version} ->
%%     %        File = filename:join([PackagesPath, atom_to_list(AllInOne) ++ Version ++ ".tar.gz"]),
            
%%     maps:fold(
%%       fun(Host, Nodes, ok) ->
%%               Opts = [{host, Host}, {current_host, CurrentHost}],
%%               case cluster_booter_cmd:cmd(exists, [{base_dir, Root}], Opts) of
%%                   true ->
%%                       ok;
%%                   false ->
%%                       sync_root_dir(Root, Opts)
%%               end,
%%               lists:foldl(
%%                 fun(Node, ok) ->
%%                         ClientDir = filename:join([Root, "clients", Node]),
%%                         case cluster_booter_cmd:cmd(exists, [{base_dir, ClientDir}], Opts) of
%%                             true ->
%%                                 ok;
%%                             false ->
%%                                 sync_client_dir(Root, Node, Opts),
%%                                 ok
%%                         end
%%                 end, ok, Nodes)
%%       end, ok, Hosts).

sync_root_dir(Root, Options) ->
    RsyncOptions = "-e clients",
    cluster_booter_cmd:cmd({rsync, ".", Root, RsyncOptions}, Options).

sync_client_dir(Root, Node, Options) ->
    From = filename:join(["clients", Node]),
    To = filename:join([Root, "clients", Node]),
    cluster_booter_cmd:cmd({rsync, From, To, ""}, Options).
