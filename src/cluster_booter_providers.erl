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
-export([add_hook/4, add_provider/3]).
-export([run/3]).

%%%===================================================================
%%% API
%%%===================================================================
add_hook(PrePost, ProviderName, HookName, Providers) when (PrePost == pre) or (PrePost == post)->
    case providers:get_provider(ProviderName, Providers) of
        not_found ->
            {error, {provider_not_found, ProviderName}};
        Provider ->
            case providers:get_provider(HookName, Providers) of
                not_found ->
                    {error, hook_not_found, HookName};
                _ ->
                    {PreHooks, PostHooks} = providers:hooks(Provider),
                    {NPreHooks, NPostHooks} = do_add_hook(PrePost, HookName, PreHooks, PostHooks),
                    NProvider = providers:hooks(Provider, {NPreHooks, NPostHooks}),
                    NProviders = add_provider(NProvider, Providers, []),
                    {ok, NProviders}
            end
    end;
add_hook(PrePost, ProviderName, HookName, _Providers) ->
    {error, {invalid_hook, PrePost, ProviderName, HookName}}.

do_add_hook(pre, Hook, PreHooks, PostHooks) ->
    {PreHooks ++ [Hook], PostHooks};
do_add_hook(post, Hook, PreHooks, PostHooks) ->
    {PreHooks, PostHooks ++ [Hook]}.

add_provider(Provider, [CProvider|Rest], Acc) ->
    case same_provider(Provider, CProvider) of
        true ->
            lists:reverse(Acc) ++ [Provider|Rest];
        false ->
            add_provider(Provider, Rest, [CProvider|Acc])
    end;
add_provider(Provider, [], Acc) ->
    [Provider|lists:reverse(Acc)].

same_provider(P1, P2) ->
    (providers:impl(P1) == providers:impl(P2)) and (providers:namespace(P1) == providers:namespace(P2)).

run(Actions, AllProviders, State) ->
    TargetProviders = 
        lists:flatmap(fun(Target) ->
                              providers:get_target_providers(Target, AllProviders)
                      end, Actions),
    io:format("target providers is ~p~n", [TargetProviders]),
    lists:foldl(fun(ProviderName, Acc) -> run_provider(ProviderName, AllProviders, Acc) end, {ok, State}, TargetProviders). 

run_provider(ProviderName, Providers, {ok, State0}) ->
    Provider = providers:get_provider(ProviderName, Providers),
    io:format("running provider ~p~n", [ProviderName]),
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
