%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_config).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, config).
-define(DEPS, [versions]).

init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "initialize node config with template and variables."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    %% NodeVersions = cluster_booter_state:node_versions(State),
    Releases = cluster_booter_state:releases(State),
    cluster_booter_state:fold_host_nodes(
      fun(Host, Release, Node, Acc) ->
              case maps:find(Release, Releases) of
                  {ok, Version} ->
                      cluster_booter_client_config:generate(Host, Release, Version, Node, State),
                      Acc;
                  error ->
                      io:format("get version of ~p failed~n", [Node]),
                      Acc
              end
      end, ok, State),
    {ok, State}.
    
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
