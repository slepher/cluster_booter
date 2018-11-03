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
-export([node_name/1, node_name/2]).
-export([cookie/1, cookie/2]).
-export([nodes/1, nodes/2]).
-export([started_nodes/1, started_nodes/2, add_started_node/2]).
-export([unstarted_nodes/1, unstarted_nodes/2, add_unstarted_node/2]).
-export([undefined_nodes/1, undefined_nodes/2, add_undefined_node/2]).
-export([hosts/1, hosts/2]).
-export([mnesia_nodes/1, mnesia_nodes/2]).
-export([mnesia_schema/1, mnesia_schema/2]).
-export([releases/1, releases/2]).
-export([application_st/1, application_st/2]).
-export([main_application_st/1, main_application_st/2]).
-export([node_map/1, node_map/2, providers/1, providers/2, add_provider/2]).

-include_lib("providers/include/providers.hrl").

-record(state_t, {providers = [], 
                  init_providers = [],
                  added_providers = [],
                  node_name,
                  cookie,
                  nodes = [], 
                  started_nodes = [],
                  unstarted_nodes = [],
                  undefined_nodes = [],
                  node_map = maps:new(), 
                  hosts = maps:new(),
                  mnesia_nodes = maps:new(),
                  mnesia_schema,
                  releases = maps:new(),
                  application_st = maps:new(),
                  main_application_st = maps:new(),
                  allow_provider_overrides=false}).

-type t() :: #state_t{}.

%%%===================================================================
%%% API
%%%===================================================================
new() ->
    #state_t{}.

validate(_State) ->
    ok.

initialize(State) ->
    InitProviders = init_providers(State),
    AddedProviders = added_providers(State),
    Providers = 
        case InitProviders of
            [] ->
                [cluster_booter_prv_node_status,
                 cluster_booter_prv_application_status,
                 cluster_booter_prv_mnesia,
                 cluster_booter_prv_application
                ];
            _ ->
                InitProviders
        end,
    create_all_providers(State, Providers ++ AddedProviders).

load_terms(Configs, State) ->    
    lists:foldl(
      fun(Config, {ok, Acc}) ->
              load_term(Config, Acc);
         (_Config, {error, Reason}) ->
              {error, Reason}
      end, {ok, State}, Configs).

load_term({nodes, Nodes}, State) ->
    {ok, nodes(State, Nodes)};
load_term({hosts, Hosts}, State) ->
    NodeMap = 
        lists:map(
          fun({NodeName, Host}) ->
                  {NodeName, binary_to_atom(list_to_binary([atom_to_list(NodeName), "@", Host]), utf8)}
          end, Hosts),
    NState = hosts(State, Hosts),
    {ok, node_map(NState, NodeMap)};
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
load_term(_Term, State) ->
    {ok, State}.

node_name(#state_t{node_name = NodeName}) ->
    NodeName.

node_name(#state_t{} = State, NodeName) ->
    State#state_t{node_name = NodeName}.

cookie(#state_t{cookie = Cookie}) ->
    Cookie.

cookie(#state_t{} = State, Cookie) ->
    State#state_t{cookie = Cookie}.

nodes(#state_t{nodes = Nodes}) ->
    Nodes.

nodes(#state_t{} = State, Nodes) ->
    State#state_t{nodes = Nodes}.

init_providers(#state_t{init_providers = InitProviders}) ->
    InitProviders.

init_providers(#state_t{} = State, InitProviders) ->
    State#state_t{init_providers = InitProviders}.

added_providers(#state_t{added_providers = AddedProviders}) ->
    AddedProviders.

added_providers(#state_t{} = State, AddedProviders) ->
    State#state_t{added_providers = AddedProviders}.

started_nodes(#state_t{started_nodes = Nodes}) ->
    Nodes.

started_nodes(#state_t{} = State, Nodes) ->
    State#state_t{started_nodes = Nodes}.

add_started_node(#state_t{started_nodes = Nodes} = State, Node) ->
    State#state_t{started_nodes = [Node|Nodes]}.

unstarted_nodes(#state_t{unstarted_nodes = Nodes}) ->
    Nodes.

unstarted_nodes(#state_t{} = State, Nodes) ->
    State#state_t{unstarted_nodes = Nodes}.

add_unstarted_node(#state_t{unstarted_nodes = Nodes} = State, Node) ->
    State#state_t{unstarted_nodes = [Node|Nodes]}.

undefined_nodes(#state_t{undefined_nodes = Nodes}) ->
    Nodes.

undefined_nodes(#state_t{} = State, Nodes) ->
    State#state_t{undefined_nodes = Nodes}.

add_undefined_node(#state_t{undefined_nodes = Nodes} = State, Node) ->
    State#state_t{undefined_nodes = [Node|Nodes]}.

hosts(#state_t{hosts = Hosts}) ->
    Hosts.

hosts(#state_t{} = State, HostMap) when is_map(HostMap) ->
    State#state_t{node_map = HostMap};
hosts(#state_t{} = State, HostList) when is_list(HostList) ->
    HostMap = maps:from_list(HostList),
    State#state_t{hosts = HostMap}.

node_map(#state_t{node_map = NodeMap}) ->
    NodeMap.

node_map(#state_t{} = State, NodeMap) when is_map(NodeMap) ->
    State#state_t{node_map = NodeMap};
node_map(#state_t{} = State, NodeList) when is_list(NodeList) ->
    NodeMap = maps:from_list(NodeList),
    State#state_t{node_map = NodeMap}.

mnesia_nodes(#state_t{mnesia_nodes = MnesiaNodes}) ->
    MnesiaNodes.

mnesia_nodes(#state_t{} = State, MnesiaNodes) ->
    State#state_t{mnesia_nodes = MnesiaNodes}.

mnesia_schema(#state_t{mnesia_schema = MnesiaSchema}) ->
    MnesiaSchema.

mnesia_schema(#state_t{} = State, MnesiaSchema) ->
    State#state_t{mnesia_schema = MnesiaSchema}.

releases(#state_t{releases = Releases}) ->
    Releases.

releases(#state_t{} = State, Releases) ->
    State#state_t{releases = Releases}.

application_st(#state_t{application_st = Applications}) ->
    Applications.

application_st(#state_t{} = State, Applications) ->
    State#state_t{application_st = Applications}.

main_application_st(#state_t{main_application_st = Applications}) ->
    Applications.

main_application_st(#state_t{} = State, Applications) ->
    State#state_t{main_application_st = Applications}.

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

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
            lager:debug("Not adding provider ~p ~p from module ~p because it already exists from module ~p",[Namespace, Name, Module, providers:module(P)]),
            true;
        _ ->
            false
    end.
