%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jan 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_file_lib).

%% API
-export([consult_clusup/1]).
-export([copy_clusfile/1]).
-export([install/2]).

%%%===================================================================
%%% API
%%%===================================================================
consult_clusup(ClusupFile) ->
    case file:consult(ClusupFile) of
        {ok,[{clusup, ClusterName, ToVsn, FromVsn, ReleaseChanges, AppChanges}]} ->
            {ok, {clusup, ClusterName, ToVsn, FromVsn, ReleaseChanges, AppChanges, []}};
        {ok, [{clusup, ClusterName, ToVsn, FromVsn, ReleaseChanges, AppChanges, Extra}]} ->
            {ok, {clusup, ClusterName, ToVsn, FromVsn, ReleaseChanges, AppChanges, Extra}};
        {error, Reason} ->
            {error, {consult_file_failed, ClusupFile, Reason}}
    end.

copy_clusfile(State) ->
    {ok, Cwd} = file:get_cwd(),
    AllInOne = cluster_booter_state:all_in_one(State),
    ClusName = atom_to_list(AllInOne) ++ ".clus",
    UpVsn = cluster_booter_state:version(State),
    From = filename:join([Cwd, "releases", UpVsn, ClusName]),
    To = filename:join([Cwd, "releases", ClusName]),
    file:copy(From, To).

install(State, Force) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    Root = cluster_booter_state:root(State),
    Packages = cluster_booter_state:packages(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    NeedInstall = PackagesPath =/= Root,
    case get_latest_version(AllInOne, Packages) of
        {ok, Version} ->
            Result = install_package(Version, State, NeedInstall, Force),
            copy_clus_files(AllInOne, Version, PackagesPath, NeedInstall),
            delete_extracted_files(PackagesPath, AllInOne, NeedInstall),
            case Result of
                ok ->
                    cluster_booter_state:load_cluster(State, true);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_latest_version(Release, Packages) ->
    case maps:find(Release, Packages) of
        {ok, VersionsMap} ->
            Versions = maps:keys(VersionsMap),
            Version = lists:max(Versions),
            {ok, Version};
        error ->
            {error, no_usable_packages}
    end.

install_package(Version, State, true, Force) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    Hosts = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Root = cluster_booter_state:root(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    maps:fold(
      fun(Host, Nodes, ok) ->
              case extract_package(AllInOne, Version, PackagesPath) of
                  ok ->
                      Opts = [{host, Host}, {current_host, CurrentHost}],
                      case sync_root_dir(Root, Opts, AllInOne, PackagesPath, Force) of
                          ok ->
                              lists:foldl(
                                fun(Node, ok) ->
                                        sync_client_dir(Root, Node, Opts, PackagesPath, AllInOne, Force);
                                   (_Node, {error, Reason}) ->
                                        {error, Reason}
                                end, ok, Nodes);
                          {error, Reason} ->
                              {error, Reason}
                      end;
                  {error, Reason} ->
                      {error, Reason}
              end;
         (_Host, _Nodes, {error, Reason}) ->
              {error, Reason}
      end, ok, Hosts);
install_package(_Version, _State, false, _Force) ->
    ok.

extract_package(AllInOne, Version, PackagesPath) ->
    TargetDirectory = filename:join([PackagesPath, AllInOne]),
    case filelib:is_dir(TargetDirectory) of
        false ->
            File = filename:join([PackagesPath, atom_to_list(AllInOne) ++ "_" ++ Version ++ ".tar.gz"]),
            io:format("file is ~s~n", [File]),
            Result = erl_tar:extract(File, [{cwd, TargetDirectory}, compressed]),
            io:format("result is ~p~n", [Result]),
            Result;
        true ->
            ok
    end.

check_exists(Root, Opts) ->
    Cmd = cluster_booter_cmd:cmd(exists, [{base_dir, Root}], Opts),
    case os:cmd(Cmd) of
        [$o,$k|_T] ->
            true;
        _ ->
            false
    end.

skip_if_installed(Root, Opts, false) ->
    check_exists(Root, Opts);
skip_if_installed(_Root, _Opts, true) ->
    false.

sync_root_dir(Root, Opts, AllInOne, PackagesPath, Force) ->
    case skip_if_installed(Root, Opts, Force) of
        true ->
            ok;
        false ->
            TargetDirectory = filename:join([PackagesPath, AllInOne]),
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
            ok
    end.

sync_client_dir(Root, Node, Opts, PackagesPath, AllInOne, Force) ->
    FromPath = filename:join([PackagesPath, AllInOne]),
    ClientDir = filename:join([Root, "clients", Node]),
    case skip_if_installed(ClientDir, Opts, Force) of
        true ->
            ok;
        false ->
            From = filename:join([FromPath, "clients", Node]) ++ "/",
            case filelib:is_dir(From) of
                true ->
                    To = filename:join([Root, "clients", Node]) ++ "/",
                    case check_exists(To, Opts) of
                        false ->
                            Cmd2 = cluster_booter_cmd:cmd(mkdir, [{dir, To}], Opts),
                            os:cmd(Cmd2);
                        true ->
                            ok
                    end,
                    Cmd3 = cluster_booter_cmd:cmd({rsync, From, To, ""}, Opts),
                    os:cmd(Cmd3),
                    LibDir = filename:join(To, "lib"),
                    LinkMnesiaDir = filename:join(["..", "..", "mnesia", Node]),
                    LinkMnesiaDir2 = filename:join([Root, "mnesia", Node]),
                    MnesiaDir = filename:join(To, "mnesia"),
                    case check_exists(LibDir, Opts) of
                        true ->
                            Cmd4 = cluster_booter_cmd:cmd("ln -s ../../lib " ++ LibDir, Opts),
                            os:cmd(Cmd4);
                        false ->
                            ok
                    end,
                    case check_exists(LinkMnesiaDir2, Opts) of
                        false ->
                            Cmd5 = cluster_booter_cmd:cmd(mkdir, [{dir, LinkMnesiaDir2}], Opts),
                            os:cmd(Cmd5);
                        true ->
                            ok
                    end,
                    case check_exists(LinkMnesiaDir2, Opts) of
                        false ->
                            Cmd6 = cluster_booter_cmd:cmd("ln -s " ++ LinkMnesiaDir ++ " " ++ MnesiaDir, Opts),
                            os:cmd(Cmd6);
                        true ->
                            ok
                    end,
                    ok;
                false ->
                    {error, {client_not_exists, Node}}
            end
    end.

copy_clus_files(AllInOne, Version, PackagesPath, NeedInstall) ->
    ClusterName = atom_to_list(AllInOne) ++ ".clus",
    {ok, Cwd} = file:get_cwd(),
    file:make_dir(filename:join([Cwd, "releases"])),
    file:make_dir(filename:join([Cwd, "releases", Version])),
    FromName = 
        case NeedInstall of
            true ->
                filename:join([PackagesPath, atom_to_list(AllInOne), "releases", Version, ClusterName]);
            false ->
                filename:join([PackagesPath, "releases", Version, ClusterName])
        end,
    ClusterFileName = filename:join([Cwd, "releases", Version, ClusterName]),
    ClusterFileName1 = filename:join([Cwd, "releases", ClusterName]),
    file:copy(FromName, ClusterFileName),
    file:copy(FromName, ClusterFileName1),
    ok.

delete_extracted_files(PackagesPath, AllInOne, true) ->
    os:cmd(lists:flatten(io_lib:format("rm -rf ~s", [filename:join([PackagesPath, AllInOne])])));
delete_extracted_files(_PackagesPath, _AllInOne, false) ->
    ok.
