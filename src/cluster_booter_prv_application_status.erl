%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_application_status).
-behaviour(provider).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, app_status).
-define(DEPS, [node_status]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    State1 = cluster_booter_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                                        {module, ?MODULE},
                                                                        {deps, ?DEPS}])),
    {ok, State1}.

do(State) ->
    NodeNames = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    StartedNodes = cluster_booter_state:started_nodes(State),
    NodeMainApplications = cluster_booter_state:releases(State),
    case cluster_booter_application:node_applications(StartedNodes, NodeMap) of
        {ok, NodeApplications} ->
            ApplicationSt = cluster_booter_application:application_st(NodeMainApplications, NodeApplications),
            NState = cluster_booter_state:application_st(State, NodeMainApplications),
            NNState = cluster_booter_state:main_application_st(NState, ApplicationSt),
            print_application_st(ApplicationSt),
            {ok, NNState};
        {error, Reason} ->
            {error, Reason}
    end.

format_error({undefined_nodes, Nodes}) when is_list(Nodes) ->
    NodesStr = string:join(lists:map(fun(Node) -> atom_to_list(Node) end, Nodes), ","),
    io_lib:format("Nodes not configured ~s", [NodesStr]).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
print_application_st(ApplicationSt) ->
    maps:fold(
      fun({Node, App}, not_started, ok) ->
              io:format("application ~p not started at ~p.~n", [App, Node]);
         ({Node, App}, Vsn, ok) ->
              io:format("application ~p-~s started at ~p.~n", [App, Vsn, Node])
      end, ok, ApplicationSt).