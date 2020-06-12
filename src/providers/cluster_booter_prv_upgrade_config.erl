%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_upgrade_config).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, upgrade_config).
-define(DEPS, [install_upgrade]).

init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "initialize node config with template and variables."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    case upgrade_changes(State) of
        {ok, Changes} ->
            Releases = upgrade_versions(Changes),
            cluster_booter_state:fold_host_nodes(
              fun(Host, Release, Node, Acc) ->
                      case maps:find(Release, Releases) of
                          {ok, Version} ->
                              io:format("generate config of ~p ~s~n", [Release, Version]),
                              cluster_booter_client_config:generate(Host, Release, Version, Node, State),
                              Acc;
                          error ->
                              Acc
                      end
              end, ok, State),
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.
    
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
upgrade_versions(Changes) ->
    lists:foldl(
      fun({change, NodeName, Vsn, _FromVsn}, Acc) ->
              maps:put(NodeName, Vsn, Acc);
         ({add, NodeName, Vsn}, Acc) ->
              maps:put(NodeName, Vsn, Acc);
         ({remove, _NodeName, _Vsn}, Acc) ->
              Acc
      end, maps:new(), Changes).

upgrade_changes(State) ->
    PackagesPath = cluster_booter_state:packages_path(State),
    ClusupPath = filename:join([PackagesPath, "clusup"]),
    case file:consult(ClusupPath) of
        {ok,[{clusup, _ClusterName, Changes}]} ->
            {ok, Changes};
        {ok, [{clusup, _ClusterName, Changes, _Extra}]} ->
            {ok, Changes};
        {error, Reason} ->
            {error, Reason}
    end.
