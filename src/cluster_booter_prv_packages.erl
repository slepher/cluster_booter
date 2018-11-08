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
-export([packages/1]).

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
    PackagePath = cluster_booter_state:packages_path(State),
    Packages = packages(PackagePath),
    format_packages(Packages),
    NState = cluster_booter_state:packages(State, Packages),
    {ok, NState}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%%%===================================================================
%%% API
%%%===================================================================
packages(PackagePath) ->
    Files = filelib:wildcard("*.tar.gz", PackagePath),
    lists:foldl(
      fun(Filename, Acc) ->
              Basename = filename:basename(Filename, ".tar.gz"),
              case string:split(Basename, "-") of
                  [ReleaseName, Version] ->
                      VersionMap = maps:get(ReleaseName, Acc, maps:new()),
                      NVersionMap = maps:put(Version, filename:join(PackagePath, Filename), VersionMap),
                      maps:put(list_to_atom(ReleaseName), NVersionMap, Acc);
                  _ ->
                      Acc
              end
      end, maps:new(), Files).
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
                  
                        
