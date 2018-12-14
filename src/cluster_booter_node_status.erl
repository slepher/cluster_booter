%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Dec 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_node_status).

%% API
-export([new/0, check/2]).

-record(node_status, {started_nodes = [],
                      unstarted_nodes = [],
                      undefined_nodes = []
                     }).

-make_lenses([node_status]).


new() ->
    #node_status{}.

%%%===================================================================
%%% API
%%%===================================================================

check(NodeNames, NodeMap) ->
    NodeStatus = new(),
    lists:foldl(
      fun(NodeName, StateAcc) ->
              case maps:find(NodeName, NodeMap) of
                  {ok, Node} ->
                      case net_adm:ping(Node) of
                          pong ->
                              add_started_node(NodeName, StateAcc);
                          pang ->
                              add_unstarted_node(NodeName, StateAcc)
                      end;
                  error ->
                      add_undefined_node(NodeName, StateAcc)
              end
      end, NodeStatus, NodeNames).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_started_node(Node, #node_status{started_nodes = Nodes} = NodeStatus) ->
    NodeStatus#node_status{started_nodes = [Node|Nodes]}.

add_unstarted_node(Node, #node_status{unstarted_nodes = Nodes} = NodeStatus) ->
    NodeStatus#node_status{unstarted_nodes = [Node|Nodes]}.

add_undefined_node(Node, #node_status{undefined_nodes = Nodes} = NodeStatus) ->
    NodeStatus#node_status{undefined_nodes = [Node|Nodes]}.
