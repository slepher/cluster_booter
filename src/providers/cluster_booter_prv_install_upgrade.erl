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
    do_all_in_one(AllInOne, State).

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_all_in_one(AllInOne, State) ->
    Root = cluster_booter_state:root(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    case Root == PackagesPath of
        true ->
            io:format("root path and package path is same, no need to install upgrade~n"),
            {ok, State};
        false ->
            case install_packages(AllInOne, State) of
                ok ->
                    cluster_booter_prv_installed_packages:do(State);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

install_packages(AllInOne, State) ->
    Hosts = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Root = cluster_booter_state:root(State),
    Packages = cluster_booter_state:upgrade_packages(State),
    PackagesPath = cluster_booter_state:packages_path(State),
    io:format("packages path is ~s~n", [PackagesPath]),
    case extract_package(AllInOne, PackagesPath, Packages) of
        ok ->
            case get_upgrade_clients(PackagesPath) of
                {ok, ChangeClients} ->
                    Result = 
                        maps:fold(
                          fun(Host, Nodes, ok) ->
                                  Opts = [{host, Host}, {current_host, CurrentHost}],
                                  case sync_root_dir(Root, Opts, AllInOne, PackagesPath) of
                                      ok ->
                                          lists:foldl(
                                            fun(Node, ok) ->
                                                    sync_client_dir(Root, Node, Opts, ChangeClients, PackagesPath, AllInOne);
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
                    Result;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

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
                            FromFile = filename:join([TargetDirectory, "clusup"]),
                            ToFile = filename:join([PackagesPath, "clusup"]),
                            file:copy(FromFile, ToFile),
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

sync_root_dir(Root, Opts, AllInOne, PackagesPath) ->
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
    ok.

sync_client_dir(Root, Node, Opts, Clients, PackagesPath, AllInOne) ->
    case maps:find(Node, Clients) of
        {ok, Vsn} ->
            FromPath = filename:join([PackagesPath, AllInOne]),
            From = filename:join([FromPath, "clients", Node, "releases"]) ++ "/",
            FromVsn = filename:join([From, Vsn]),
            case filelib:is_dir(FromVsn) of
                true ->
                    To = filename:join([Root, "clients", Node, "releases"]) ++ "/",
                    Cmd2 = cluster_booter_cmd:cmd(mkdir, [{dir, To}], Opts),
                    os:cmd(Cmd2),
                    Cmd3 = cluster_booter_cmd:cmd({rsync, From, To, ""}, Opts),
                    os:cmd(Cmd3),
                    ok;
                false ->
                    {error, {client_not_exists, Node, Vsn}}
            end;
        error ->
            ok
    end.

clusup_clients(Changes) ->
    lists:foldl(
      fun({change, NodeName, Vsn, _FromVsn}, Acc) ->
              maps:put(NodeName, Vsn, Acc);
         ({add, NodeName, Vsn}, Acc) ->
              maps:put(NodeName, Vsn, Acc);
         (_, Acc) ->
              Acc
      end, maps:new(), Changes).
    
get_upgrade_clients(PackagesPath) ->
    ClusupPath = filename:join([PackagesPath, "clusup"]),
    case file:consult(ClusupPath) of
        {ok,[{clusup, _ClusterName, Changes}]} ->
            ChangeClients = clusup_clients(Changes),
            {ok, ChangeClients};
        {ok, [{clusup, _ClusterName, Changes, _Extra}]} ->
            ChangeClients = clusup_clients(Changes),
            {ok, ChangeClients};
        {error, Reason} ->
            {error, Reason}
    end.
