%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_install_upgrade).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, install_upgrade).
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
    Result = do_all_in_one(AllInOne, State),
    case Result of
        ok ->
            cluster_booter_prv_installed_packages:do(State);
        {error, Reason} ->
            {error, Reason}
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_all_in_one(AllInOne, State) ->
    Hosts = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Root = cluster_booter_state:root(State),
    Packages = cluster_booter_state:upgrade_packages(State),
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

extract_package(AllInOne, PackagesPath, Packages) ->
    TargetDirectory = filename:join([PackagesPath, AllInOne]),
    case filelib:is_dir(TargetDirectory) of
        false ->
            case get_latest_version(AllInOne, Packages) of
                {ok, Version} ->
                    case get_latest_upgrade_version(AllInOne, Packages, Version) of
                        {ok, FromVersion} ->
                            File = filename:join([PackagesPath, atom_to_list(AllInOne) ++ 
                                                      "_" ++ FromVersion ++ "-" ++ Version ++ ".tar.gz"]),
                            io:format("file is ~s~n", [File]),
                            Result = erl_tar:extract(File, [{cwd, TargetDirectory}, compressed]),
                            io:format("result is ~p~n", [Result]),
                            Result;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        true ->
            ok
    end.

get_latest_version(Release, Packages) ->
    case maps:find(Release, Packages) of
        {ok, VersionsMap} ->
            Versions = maps:keys(VersionsMap),
            Version = lists:max(Versions),
            {ok, Version};
        error ->
            {error, no_usabe_packages}
    end.

get_latest_upgrade_version(Release, Packages, To) ->
    case maps:find(Release, Packages) of
        {ok, VersionsMap} ->
            case maps:find(To, VersionsMap) of
                {ok, FromVersionsMap} ->
                    Versions = maps:keys(FromVersionsMap),
                    Version = lists:max(Versions),
                    {ok, Version};
                error ->
                    {error, no_usable_packages}
            end;
        error ->
            {error, no_usabe_packages}
    end.

sync_root_dir(Root, Opts, AllInOne, PackagesPath, Packages) ->
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
            Cmd2 = cluster_booter_cmd:cmd(mkdir, [{dir, To}], Opts),
            os:cmd(Cmd2),
            Cmd3 = cluster_booter_cmd:cmd({rsync, TargetDirectory ++ "/", To, RsyncOptions}, Opts),
            os:cmd(Cmd3),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

sync_client_dir(Root, Node, Opts, PackagesPath, AllInOne) ->
    FromPath = filename:join([PackagesPath, AllInOne]),
    ClientDir = filename:join([Root, "clients", Node]),
    Cmd = cluster_booter_cmd:cmd(exists, [{base_dir, ClientDir}], Opts),
    case os:cmd(Cmd) of
        [$o,$k|_T] ->
            To = filename:join([Root, "clients", Node]) ++ "/",
            LibDir = filename:join(To, "lib"),
            LinkMnesiaDir = filename:join(["..", "..", "mnesia", Node]),
            LinkMnesiaDir2 = filename:join([Root, "mnesia", Node]),
            MnesiaDir = filename:join(To, "mnesia"),
            case filelib:is_file(LibDir) of
                true ->
                    ok;
                false ->
                    Cmd2 = cluster_booter_cmd:cmd("ln -s ../../lib " ++ LibDir, Opts),
                    os:cmd(Cmd2)
            end,
            case filelib:is_file(LinkMnesiaDir2) of
                true ->
                    ok;
                false ->
                    Cmd3 = cluster_booter_cmd:cmd(mkdir, [{dir, LinkMnesiaDir2}], Opts),
                    os:cmd(Cmd3)
            end,
            case filelib:is_file(MnesiaDir) of
                true ->
                    ok;
                false ->
                    Cmd4 = cluster_booter_cmd:cmd("ln -s " ++ LinkMnesiaDir ++ " " ++ MnesiaDir, Opts),
                    os:cmd(Cmd4)
            end,
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
