%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_state).

%% API
-export([new/0]).
-export([validate/1]).
-export([load_terms/2]).
-export([initialize/1]).
-export([get_env/2, get_env/3]).
-export([clear_node_status/1, add_started_node/2, add_unstarted_node/2, add_undefined_node/2]).
-export([add_provider/2, add_to_provider_hooks/3]).
-export([cmd_opt/2]).
-export([fold_host_nodes/3, installed/3]).
-export([node_started/2]).

-include_lib("providers/include/providers.hrl").

-compile({parse_transform, make_lenses}).

-record(state_t, {providers = [], 
                  init_providers = [],
                  added_providers = [],
                  provider_hooks = [],
                  current_host = "localhost",
                  packages_path = ".",
                  packages = maps:new(),
                  installed_packages = maps:new(),
                  root,
                  env = [],
                  node_name,
                  cookie,
                  nodes = [], 
                  release_nodes_map = maps:new(),
                  node_release_map = maps:new(),
                  started_nodes = [],
                  unstarted_nodes = [],
                  undefined_nodes = [],
                  running_processes = [],
                  node_map = maps:new(), 
                  hosts = maps:new(),
                  mnesia_nodes = maps:new(),
                  mnesia_schema,
                  releases = maps:new(),
                  application_st = maps:new(),
                  main_application_st = maps:new(),
                  allow_provider_overrides=false}).

-type t() :: #state_t{}.

-make_lenses([state_t]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #state_t{}.

validate(_State) ->
    ok.

cmd_opt(Host, #state_t{current_host = CurrentHost}) ->
    [{host, Host}, {current_host, CurrentHost}].

    

initialize(State) ->
    InitProviders = init_providers(State),
    AddedProviders = added_providers(State),
    Providers = 
        case InitProviders of
            [] ->
                [cluster_booter_prv_node_status,
                 cluster_booter_prv_application_status,
                 cluster_booter_prv_mnesia,
                 cluster_booter_prv_application,
                 cluster_booter_prv_process,
                 cluster_booter_prv_packages,
                 cluster_booter_prv_installed_packages,
                 cluster_booter_prv_install_packages,
                 cluster_booter_prv_start_node,
                 cluster_booter_prv_initialize
                ];
            _ ->
                InitProviders
        end,
    case create_all_providers(State, Providers ++ AddedProviders) of
        {ok, NState} ->
            init_provider_hooks(NState);
        {error, Reason} ->
            {error, Reason}
    end.

load_terms(Configs, State) ->    
    lists:foldl(
      fun(Config, {ok, Acc}) ->
              load_term(Config, Acc);
         (_Config, {error, Reason}) ->
              {error, Reason}
      end, {ok, State}, Configs).

load_term({nodes, NodeList}, State) ->
    {ReleaseNodesMap, NodeReleaseMap, NNodes} = 
        lists:foldl(
          fun({Release, Nodes}, {AccReleaseNodesMap, AccNodeReleaseMap, AccNodes}) ->
                  NAccReleaseNodesMap = maps:put(Release, Nodes, AccReleaseNodesMap),
                  NAccNodeReleaseMap = 
                      lists:foldl(
                        fun(Node, Acc) ->
                                maps:put(Node, Release, Acc)
                        end, AccNodeReleaseMap, Nodes),
                  NAccNodes = Nodes ++ AccNodes,
                  {NAccReleaseNodesMap, NAccNodeReleaseMap, NAccNodes};
             (Release, {AccReleaseNodesMap, AccNodeReleaseMap, AccNodes}) when is_atom(Release) ->
                  NAccReleaseNodesMap = maps:put(Release, [Release], AccReleaseNodesMap),
                  NAccNodeReleaseMap = maps:put(Release, Release, AccNodeReleaseMap),
                  NAccNodes = [Release|AccNodes],
                  {NAccReleaseNodesMap, NAccNodeReleaseMap, NAccNodes}
          end, {maps:new(), maps:new(), []}, NodeList),
    NState = release_nodes_map(State, ReleaseNodesMap),
    NNState = node_release_map(NState, NodeReleaseMap),
    {ok, nodes(NNState, NNodes)};
load_term({hosts, Hosts}, State) when is_list(Hosts) ->
    NodeMap = 
        lists:foldl(
          fun({Host, Nodes}, Acc) ->
                  lists:foldl(
                    fun(NodeName, Acc1) ->
                            [{NodeName, cluster_booter_node:node(NodeName, Host)}|Acc1]
                    end, Acc, Nodes)
          end, [], Hosts),
    NHosts = maps:from_list(Hosts),
    NNodeMap = maps:from_list(NodeMap),
    NState = hosts(State, NHosts),
    {ok, node_map(NState, NNodeMap)};
load_term({mnesia_nodes, Nodes}, State) when is_list(Nodes) ->
    MnesiaNodeMap = maps:from_list(Nodes),
    NState = mnesia_nodes(State, MnesiaNodeMap),
    {ok, NState};
load_term({mnesia_schema, MnesiaSchema}, State) ->
    NState = mnesia_schema(State, MnesiaSchema),
    {ok, NState};
load_term({releases, Applications}, State) ->
    NState = releases(State, maps:from_list(Applications)),
    {ok, NState};
load_term({node_name, NodeName}, State) ->
    NState = node_name(State, NodeName),
    {ok, NState};
load_term({cookie, Cookie}, State) ->
    NState = cookie(State, Cookie),
    {ok, NState};
load_term({providers, Providers}, State) ->
    NState = init_providers(State, Providers),
    {ok, NState};
load_term({add_providers, Providers}, State) ->
    NState = added_providers(State, Providers),
    {ok, NState};
load_term({root, Root}, State) ->
    NState = root(State, Root),
    {ok, NState};
load_term({current_host, CurrentHost}, State) ->
    NState = current_host(State, CurrentHost),
    {ok, NState};
load_term({packages_path, PackagePath}, State) ->
    NState = packages_path(State, PackagePath),
    {ok, NState};
load_term({hooks, Hooks}, State) ->
    NState = provider_hooks(State, Hooks),
    {ok,NState};
load_term({env, Env}, State) ->
    NState = env(State, Env),
    {ok, NState};
load_term(_Term, State) ->
    {ok, State}.

get_env(Key, #state_t{env = Env} = State) ->
    get_env(Key, State, undefined).

get_env(Key, #state_t{env = Env} = State, Default) ->
    proplists:get_value(Key, Env, Default).


-spec add_provider(t(), providers:t()) -> t().
add_provider(State=#state_t{providers=Providers, allow_provider_overrides=true}, Provider) ->
    State#state_t{providers=[Provider | Providers]};
add_provider(State=#state_t{providers=Providers, allow_provider_overrides=false}, Provider) ->
    Name = providers:impl(Provider),
    Namespace = providers:namespace(Provider),
    Module = providers:module(Provider),
    case lists:any(fun(P) ->
                           provider_exists(P, Module, Name, Namespace)
                   end, Providers) of
        true ->
            State;
        false ->
            State#state_t{providers=[Provider | Providers]}
    end.

add_to_provider_hooks(HookName, {HookType, ProviderName}, #state_t{provider_hooks = ProviderHooks} = State) ->
    State#state_t{provider_hooks = ProviderHooks ++ [{HookType, ProviderName, HookName}]}.

init_provider_hooks(#state_t{providers = Providers, provider_hooks = Hooks} = State) ->
    case lists:foldl(
           fun({PrePost, Name, Hook}, {ok, ProvidersAcc}) ->
                   cluster_booter_providers:add_hook(PrePost, Name, Hook, ProvidersAcc);
              (_, {error, Reason}) ->
                   {error, Reason}
           end, {ok, Providers}, Hooks) of
        {ok, NProviders} ->
            {ok, State#state_t{providers = NProviders}};
        {error, Reason} ->
            {error, Reason}
    end.

clear_node_status(#state_t{} = State) ->
    State#state_t{started_nodes = [], unstarted_nodes = [], undefined_nodes = []}.

add_started_node(#state_t{started_nodes = Nodes} = State, Node) ->
    State#state_t{started_nodes = [Node|Nodes]}.

add_unstarted_node(#state_t{unstarted_nodes = Nodes} = State, Node) ->
    State#state_t{unstarted_nodes = [Node|Nodes]}.

add_undefined_node(#state_t{undefined_nodes = Nodes} = State, Node) ->
    State#state_t{undefined_nodes = [Node|Nodes]}.

fold_host_nodes(Fun, Init, #state_t{hosts = Hosts, node_release_map = NodeReleases}) ->
    maps:fold(
      fun(Host, Nodes, Acc) ->
              lists:foldl(
                fun(Node, Acc1) ->
                        case maps:find(Node, NodeReleases) of
                            {ok, Release} ->
                                Fun(Host, Release, Node, Acc1);
                            error ->
                                Acc1
                        end
                end, Acc, Nodes)
      end, Init, Hosts).

installed(Host, Release, #state_t{installed_packages = InstalledPackages}) ->
   case maps:find(Host, InstalledPackages) of
       {ok, HostInstalled} ->
           case maps:find(Release, HostInstalled) of
               {ok, Installed} ->
                   Installed;
               error ->
                   unknown
           end;
       error ->
           unknown
   end.

node_started(NodeName, #state_t{started_nodes = StartedNodes}) ->
    lists:member(NodeName, StartedNodes).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_all_providers(State, []) ->
    {ok, State};
create_all_providers(State, [Module | Rest]) ->
    case providers:new(Module, State) of
        {ok, State1} ->
            create_all_providers(State1, Rest);
        Error ->
             Error
    end.

provider_exists(P, Module, Name, Namespace) ->
    case {providers:impl(P), providers:namespace(P)} of
        {Name, Namespace} ->
            io:format("Not adding provider ~p ~p from module ~p because it already exists from module ~p",[Namespace, Name, Module, providers:module(P)]),
            true;
        _ ->
            false
    end.
