%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_state).

-include_lib("astranaut/include/rebinding.hrl").

%% API
-export([new/0]).
-export([validate/1, transform/1]).
-export([create_all_providers/2]).
-export([load_terms/2, load_cluster/1]).
-export([initialize/1]).
-export([get_env/2, get_env/3, get_node/2]).
-export([add_provider/2, add_to_provider_hooks/3]).
-export([cmd_opt/2]).
-export([fold_host_nodes/3, installed/3]).
-export([node_started/2]).

-compile({parse_transform, make_lenses}).

-record(state_t, {providers = [], 
                  init_providers = [],
                  added_providers = [],
                  provider_hooks = [],
                  current_host = "localhost",
                  packages_path = ".",
                  mnesia_dir = ".",
                  log_dir = ".",
                  pipe_dir = ".",
                  packages = maps:new(),
                  installed_packages = maps:new(),
                  node_versions = maps:new(),
                  root,
                  env = [],
                  variables = maps:new(),
                  variable_opts = [],
                  node_variables = maps:new(),
                  sys_config,
                  vm_args,
                  erl_env,
                  cluster_name,
                  node_name,
                  cookie,
                  nodes = [], 
                  release_nodes_map = maps:new(),
                  node_release_map = maps:new(),
                  node_status,
                  running_processes = [],
                  node_map = maps:new(), 
                  hosts = maps:new(),
                  mnesia_nodes = maps:new(),
                  mnesia_schema,
                  all_in_one = false,
                  main_applications = maps:new(),
                  releases = maps:new(),
                  applications = maps:new(),
                  application_st = maps:new(),
                  main_application_st = maps:new(),
                  version,
                  allow_provider_overrides=false}).

-type t() :: #state_t{}.

-make_lenses([state_t]).

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #state_t{}.

transform(State) ->
    transform_variables(State).
    
transform_variables(State) ->
    Variables = variables(State),
    Env = env(State),
    VariableOpts = cluster_booter_state:variable_opts(State),
    Variables1 = cluster_booter_variable_generator:variables(Env, VariableOpts),
    Variables2 = maps:merge(Variables1, Variables),
    {ok, variables(State, Variables2)}.

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
                 cluster_booter_prv_start_nodes,
                 cluster_booter_prv_initialize,
                 cluster_booter_prv_versions,
                 cluster_booter_prv_config,
                 cluster_booter_prv_stop_nodes
                ];
            _ ->
                InitProviders
        end,
    case create_all_providers(State, Providers ++ AddedProviders) of
        {ok, State} ->
            init_provider_hooks(State);
        {error, Reason} ->
            {error, Reason}
    end.

load_cluster(State) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    ClusterFile = atom_to_list(AllInOne) ++ ".clus",
    case file:consult(ClusterFile) of
        {ok, [{cluster, ClusterName, ClusterVersion, Releases, Applications}]} ->
            {ReleaseMap, MainAppMap} = 
                lists:foldl(
                  fun({ReleaseName, ReleaseVsn, MainApps}, {VsnAcc, MainAppsAcc}) ->
                          VsnAcc1 = maps:put(ReleaseName, ReleaseVsn, VsnAcc),
                          MainAppsAcc1 = maps:put(ReleaseName, MainApps, MainAppsAcc),
                          {VsnAcc1, MainAppsAcc1}
                  end, {maps:new(), maps:new()}, Releases),
            ApplicationMap = 
                lists:foldl(
                  fun({AppName, AppVsn}, Acc) ->
                          maps:put(AppName, AppVsn, Acc)
                  end, maps:new(), Applications),
            State = cluster_booter_state:cluster_name(State, ClusterName),
            State = cluster_booter_state:version(State, ClusterVersion),
            State = cluster_booter_state:releases(State, ReleaseMap),
            State = cluster_booter_state:main_applications(State, MainAppMap),
            State = cluster_booter_state:applications(State, ApplicationMap),
            {ok, State};
        {error, enoent} ->
            {error, {load_cluster_file_failed, ClusterFile}};
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
    case file:get_cwd() of
        {ok, Cwd} ->
            Root1 = filename:absname_join(Cwd, Root),
            NState = root(State, Root1),
            {ok, NState};
        {error, Reason} ->
            {error, Reason}
    end;
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
load_term({version, Version}, State) ->
    State1 = version(State, Version),
    {ok, State1};
load_term({variables, Variables}, State) ->
    NVariables = maps:from_list(Variables),
    NState = variables(State, NVariables),
    {ok, NState};
load_term({variable_opts, Opts}, State) ->
    NState = variable_opts(State, Opts),
    {ok, NState};
load_term({node_variables, NodeVariables}, State) ->
    NNodeVariables = 
        lists:foldl(
          fun({Node, Variables}, Acc) ->
                  maps:put(Node, maps:from_list(Variables), Acc)
          end, maps:new(), NodeVariables),
    NState = node_variables(State, NNodeVariables),
    {ok, NState};
load_term({sys_config, SysConfig}, State) ->
    NState = sys_config(State, SysConfig),
    {ok, NState};
load_term({vm_args, VmArgs}, State) ->
    NState = vm_args(State, VmArgs),
    {ok, NState};
load_term({erl_env, ErlEnv}, State) ->
    NState = erl_env(State, ErlEnv),
    {ok, NState};
load_term({log_dir, LogDir}, State) ->
    NState = log_dir(State, LogDir),
    {ok, NState};
load_term({mnesia_dir, MnesiaDir}, State) ->
    NState = mnesia_dir(State, MnesiaDir),
    {ok, NState};
load_term({pipe_dir, PipeDir}, State) ->
    NState = pipe_dir(State, PipeDir),
    {ok, NState};
load_term({all_in_one, AllInOne}, State) when is_atom(AllInOne) ->
    NState = all_in_one(State, AllInOne),
    {ok, NState};
load_term(_Term, State) ->
    {ok, State}.

get_env(Key, #state_t{} = State) ->
    get_env(Key, State, undefined).

get_env(Key, #state_t{env = Env}, Default) ->
    proplists:get_value(Key, Env, Default).

get_node(NodeName, #state_t{} = State) ->
    NodeMap = cluster_booter_state:node_map(State),
    maps:get(NodeName, NodeMap).

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
        {ok, Providers} ->
            {ok, State#state_t{providers = Providers}};
        {error, Reason} ->
            {error, Reason}
    end.

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

installed(Host, Node, #state_t{installed_packages = InstalledPackages}) ->
   case maps:find(Host, InstalledPackages) of
       {ok, HostInstalled} ->
           case maps:find(Node, HostInstalled) of
               {ok, Installed} ->
                   Installed;
               error ->
                   unknown
           end;
       error ->
           unknown
   end.

node_started(NodeName, #state_t{node_status = NodeStatus}) ->
    cluster_booter_node:started(NodeName, NodeStatus).

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
        {ok, State} ->
            create_all_providers(State, Rest);
        Error ->
             Error
    end.

provider_exists(P, Module, Name, Namespace) ->
    case {providers:impl(P), providers:namespace(P)} of
        {+Name, +Namespace} ->
            io:format("Not adding provider ~p ~p from module ~p because it already exists from module ~p", 
                      [Namespace, Name, Module, providers:module(P)]),
            true;
        _ ->
            false
    end.
