%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_packages).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, packages).
-define(DEPS, []).

-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "show packages in packages_path."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    Root = cluster_booter_state:root(State),
    PackagePath = cluster_booter_state:packages_path(State),
    NeedInstall = Root =/= PackagePath,
    {Packages, UpgradePackages} = packages(AllInOne, PackagePath, NeedInstall),
    format_packages(Packages),
    format_upgrade_packages(UpgradePackages),
    State1 = cluster_booter_state:packages(State, Packages),
    State2 = cluster_booter_state:upgrade_packages(State1, UpgradePackages),
    {ok, State2}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%%%===================================================================
%%% API
%%%===================================================================
packages(_Release, PackagePath, true) ->
    Files = filelib:wildcard("*.tar.gz", PackagePath),
    lists:foldl(
      fun(Filename, {PAcc, UAcc} = Acc) ->
              Basename = filename:basename(Filename, ".tar.gz"),
              case string:split(Basename, "_", trailing) of
                  [ReleaseName, Version] ->
                      Release = list_to_atom(ReleaseName),
                      case string:split(Version, "-") of
                          [From, To] ->
                              VersionMap = maps:get(Release, UAcc, maps:new()),
                              FromMap = maps:get(To, VersionMap, maps:new()),
                              FromMap1 = maps:put(From, filename:join(PackagePath, Filename), FromMap),
                              VersionMap1 = maps:put(To, FromMap1, VersionMap),
                              UAcc1 = maps:put(Release, VersionMap1, UAcc),
                              {PAcc, UAcc1};
                          _ ->
                              VersionMap = maps:get(Release, PAcc, maps:new()),
                              VersionMap1 = maps:put(Version, filename:join(PackagePath, Filename), VersionMap),
                              PAcc1 = maps:put(Release, VersionMap1, PAcc),
                              {PAcc1, UAcc}
                      end;
                  _ ->
                      Acc
              end
      end, {maps:new(), maps:new()}, Files);
packages(Release, PackagePath, false) ->
    Dirname = filename:join([PackagePath, "releases"]),
    {ok, Files} = file:list_dir(Dirname),
    Packages = 
        lists:foldl(
          fun(Basename, Acc) ->
                  Filename = filename:join([Dirname, Basename]),
                  case filelib:is_dir(Filename) of
                      true ->
                          maps:put(Basename, Filename, Acc);
                      false ->
                          Acc
                  end
          end, maps:new(), Files),
    {maps:put(Release, Packages, maps:new()), maps:new()}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_packages(Packages) ->
    maps:fold(
      fun(Package, VersionMap, ok) ->
              Versions = maps:keys(VersionMap),
              io:format("~p:~s~n", [Package, string:join(lists:usort(Versions), ",")]),
              ok
      end, ok, Packages).
                        
format_upgrade_packages(Packages) ->
    maps:fold(
      fun(Package, VersionMap, ok) ->
              maps:fold(
                fun(To, FromMap, ok) ->
                        Versions = maps:keys(FromMap),
                        io:format("~p:~s from ~s~n", [Package, To, string:join(lists:usort(Versions), ",")]),
                        ok
                end, ok, VersionMap)
      end, ok, Packages).
