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
    Result =  
        case AllInOne of
            false ->
                do_packages(State);
            AllInOne ->
                do_all_in_one(AllInOne, State)
        end,
    case Result of
        ok ->
            cluster_booter_prv_installed_packages:do(State);
        {error, Reason} ->
            {error, Reason}
    end.

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
do_packages(State) ->
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
      end, ok, Hosts).

do_all_in_one(AllInOne, State) ->
    Hosts = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Root = cluster_booter_state:root(State),
    Packages = cluster_booter_state:packages(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    Result = 
        maps:fold(
          fun(Host, Nodes, ok) ->
                  Opts = [{host, Host}, {current_host, CurrentHost}],
                  case sync_root_dir(Root, Opts, AllInOne, PackagesPath, Packages) of
                      ok ->
                          lists:foldl(
                            fun(Node, ok) ->
                                    sync_client_dir(Root, Node, Opts, PackagesPath, AllInOne);
                               (_Node, {error, Reason}) ->
                                    {error, Reason}
                            end, ok, Nodes);
                      {error, Reason} ->
                          {error, Reason}
                  end;
             (_Host, _Nodes, {error, Reason}) ->
                  {error, Reason}
          end, ok, Hosts),
    os:cmd(lists:flatten(io_lib:format("rm -rf ~s", [filename:join([PackagesPath, AllInOne])]))),
    Result.

sync_root_dir(Root, Opts, AllInOne, PackagesPath, Packages) ->
    Cmd = cluster_booter_cmd:cmd(exists, [{base_dir, Root}], Opts),
    case os:cmd(Cmd) of
        "ok\n" ->
            ok;
        _ ->
            TargetDirectory = filename:join([PackagesPath, AllInOne]),
            case extract_package(AllInOne, PackagesPath, Packages) of
                ok ->
                    RsyncOptions = "--exclude=clients",
                    To =
                        case lists:nth(length(Root), Root) of
                            $/ ->
                                Root;
                            _ ->
                                Root ++ "/"
                        end,
                    Cmd2 = cluster_booter_cmd:cmd({rsync, TargetDirectory ++ "/", To, RsyncOptions}, Opts),
                    os:cmd(Cmd2),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

sync_client_dir(Root, Node, Opts, PackagesPath, AllInOne) ->
    FromPath = filename:join([PackagesPath, AllInOne]),
    ClientDir = filename:join([Root, "clients", Node]),
    Cmd = cluster_booter_cmd:cmd(exists, [{base_dir, ClientDir}], Opts),
    case os:cmd(Cmd) of
        "ok\n" ->
            ok;
        _ ->
            From = filename:join([FromPath, "clients", Node]) ++ "/",
            case filelib:is_dir(From) of
                true ->
                    To = filename:join([Root, "clients", Node]) ++ "/",
                    Cmd2 = cluster_booter_cmd:cmd(mkdir, [{dir, To}], Opts),
                    os:cmd(Cmd2),
                    Cmd3 = cluster_booter_cmd:cmd({rsync, From, To, ""}, Opts),
                    os:cmd(Cmd3),
                    LibDir = filename:join(To, "lib"),
                    LinkMnesiaDir = filename:join(["..", "..", "mnesia", Node]),
                    LinkMnesiaDir2 = filename:join([Root, "mnesia", Node]),
                    MnesiaDir = filename:join(To, "mnesia"),
                    Cmd4 = cluster_booter_cmd:cmd("ln -s ../../lib " ++ LibDir, Opts),
                    os:cmd(Cmd4),
                    Cmd5 = cluster_booter_cmd:cmd(mkdir, [{dir, LinkMnesiaDir2}], Opts),
                    os:cmd(Cmd5),
                    Cmd6 = cluster_booter_cmd:cmd("ln -s " ++ LinkMnesiaDir ++ " " ++ MnesiaDir, Opts),
                    os:cmd(Cmd6),
                    ok;
                false ->
                    {error, {client_not_exists, Node}}
            end
    end.

extract_package(AllInOne, PackagesPath, Packages) ->
    TargetDirectory = filename:join([PackagesPath, AllInOne]),
    case filelib:is_dir(TargetDirectory) of
        false ->
            case get_latest_version(AllInOne, Packages) of
                {ok, Version} ->
                    File = filename:join([PackagesPath, atom_to_list(AllInOne) ++ "-" ++ Version ++ ".tar.gz"]),
                    io:format("file is ~s~n", [File]),
                    Result = erl_tar:extract(File, [{cwd, TargetDirectory}, compressed]),
                    io:format("result is ~p~n", [Result]),
                    Result;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            ok
    end.
                    
