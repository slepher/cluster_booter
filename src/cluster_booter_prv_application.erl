%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_application).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, start_apps).
-define(DEPS, [start_node, app_status]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "start erlang applications on already started nodes."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    MainApplicationSt = cluster_booter_state:main_application_st(State),
    NodeMap = cluster_booter_state:node_map(State),
    MnesiaNodes = cluster_booter_state:mnesia_nodes(State),
    case cluster_booter_mnesia:boot_mnesia(MnesiaNodes, NodeMap) of
        {ok, ok} ->
            case cluster_booter_application:boot_applications(MainApplicationSt, NodeMap) of
                {ok, ok} ->
                    {ok, State};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

format_error({application_not_started, Result}) ->
    io_lib:format("application start failed ~p", [Result]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
