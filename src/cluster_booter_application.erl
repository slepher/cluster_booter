%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_booter_application).

%% API
-export([check/1]).
-export([application_st/2, node_applications/2, boot_application/2, boot_application/3, boot_applications/2]).

%%%===================================================================
%%% API
%%%===================================================================
check(State) ->
    NodeMap = cluster_booter_state:node_map(State),
    NodeStatus = cluster_booter_state:node_status(State),
    StartedNodes = cluster_booter_node:started_nodes(NodeStatus),
    NodeMainApplications = cluster_booter_state:main_applications(State),
    case cluster_booter_application:node_applications(StartedNodes, NodeMap) of
        {ok, NodeApplications} ->
            ApplicationSt = cluster_booter_application:application_st(NodeMainApplications, NodeApplications),
            NState = cluster_booter_state:application_st(State, NodeMainApplications),
            NNState = cluster_booter_state:main_application_st(NState, ApplicationSt),
            {ok, NNState};
        {error, Reason} ->
            {error, Reason}
    end. 


node_applications(Nodes, NodeMap) ->
    {OkNodes, ErrorNodes} = 
        lists:foldl(
          fun(NodeName, {OkAcc, ErrorAcc}) ->
                  Node = maps:get(NodeName, NodeMap),
                  case cluster_booter:rpc_call(Node, application, which_applications, []) of
                      {error, Reason} ->
                          NErrorAcc = maps:put(Node, {error, Reason}, ErrorAcc),
                          {OkAcc, NErrorAcc};
                      {ok, RunningApplications} ->
                          RunningApplicationMap = 
                              lists:foldl(
                                fun({Application, _Desc, Version}, Acc) ->
                                        maps:put(Application, Version, Acc)
                                end, maps:new(), RunningApplications),
                          NOkAcc = maps:put(NodeName, RunningApplicationMap, OkAcc),
                          {NOkAcc, ErrorAcc}
                  end
          end, {maps:new(), maps:new()}, Nodes),
    case maps:size(ErrorNodes) of
        0 ->
            {ok, OkNodes};
        _ ->
            {error, {get_applications_failed, ErrorNodes}}
    end.

application_st(NodeMainApps, NodeApplicationMap) ->
    maps:fold(
      fun(Node, Apps, Acc) ->
              case maps:find(Node, NodeApplicationMap) of
                  {ok, ApplicationMap} ->
                      lists:foldl(
                        fun(App, Acc1) ->
                                case maps:find(App, ApplicationMap) of
                                    {ok, Version} ->
                                        maps:put({Node, App}, Version, Acc1);
                                    error ->
                                        maps:put({Node, App}, not_started, Acc1)
                                end
                        end, Acc, Apps);
                  error ->
                      Acc
              end
      end, maps:new(), NodeMainApps).

boot_application([NodeName, Application], NodeMap) ->
    boot_application(NodeName, Application, NodeMap).

boot_application(NodeName, Application, NodeMap) ->
    Node = maps:get(NodeName, NodeMap),
    cluster_booter:rpc_call(Node, application, ensure_all_started, [Application]).

boot_applications(ApplicationSt, NodeMap) ->
    Result = 
        maps:fold(
          fun({Node, Application}, not_started, Acc) ->
                  io:format("start application ~p at node ~p.~n", [Application, Node]),
                  case cluster_booter_application:boot_application(Node, Application, NodeMap) of
                      {ok, _} ->
                          io:format("start application ~p end at node ~p.~n", [Application, Node]),
                          Acc;
                      {error, Reason} ->
                          maps:put({Node, Application}, Reason, Acc)
                  end;
             (_NodeApp, _Vsn, Acc) ->
                  Acc
          end, maps:new(), ApplicationSt),
    case maps:size(Result) of
        0 ->
            {ok, ok};
        _ ->
            {error, {application_start_failed, Result}}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
