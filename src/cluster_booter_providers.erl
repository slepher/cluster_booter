%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_providers).

%% API
-export([run/3]).

%%%===================================================================
%%% API
%%%===================================================================
run(Actions, AllProviders, State) ->
    TargetProviders = 
        lists:flatmap(fun(Target) ->
                              providers:get_target_providers(Target, AllProviders)
                      end, Actions),
    Providers1 = lists:map(fun(P) ->
                                   providers:get_provider(P, AllProviders)
                           end, TargetProviders),
    Providers2 = providers:process_deps(Providers1, AllProviders),
    lists:foldl(fun(ProviderName, Acc) -> run_provider(ProviderName, AllProviders, Acc) end, {ok, State}, Providers2).

run_provider(ProviderName, Providers, {ok, State0}) ->
    Provider = providers:get_provider(ProviderName, Providers),
    providers:do(Provider, State0);
run_provider(_ProviderName, _Providers, Error) ->
    Error.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
