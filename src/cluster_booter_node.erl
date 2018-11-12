%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_booter_node).

%% API
-export([node/2, wait/3]).
-export([validate_nodes_started/1]).
-export([validate_nodes_exists/2, validate_nodes_started/2, validate_nodes_stopped/2]).

%%%===================================================================
%%% API
%%%===================================================================
wait(_Nodes, _NodeMap, Timeout) when Timeout < 0 ->
    ok;
wait([], __NodeMap, _Timeout) ->
    ok;
wait(Nodes, NodeMap, Timeout) ->
    timer:sleep(200),
    RestNodes = validate_nodes_stopped(Nodes, NodeMap),
    wait(RestNodes, NodeMap, Timeout - 200).

node(NodeName, Host) ->
    binary_to_atom(list_to_binary([atom_to_list(NodeName), "@", Host]), utf8).


validate_nodes_exists(Nodes, NodeMap) ->
    lists:foldl(
      fun(NodeName, Acc) ->
              case maps:find(NodeName, NodeMap) of
                  {ok, undefined} ->
                      [NodeName|Acc];
                  {ok, _Node} ->
                      Acc;
                  error ->
                      [NodeName|Acc]
              end
      end, [], Nodes).

validate_nodes_started(Nodes) ->
    lists:filter(
      fun(Node) ->
              net_adm:ping(Node) == pang
      end, Nodes).

validate_nodes_started(Nodes, NodeMap) ->
    lists:filter(
      fun(NodeName) ->
              Node = maps:get(NodeName, NodeMap),
              net_adm:ping(Node) == pang
      end, Nodes).

validate_nodes_stopped(Nodes, NodeMap) ->
    lists:filter(
      fun(NodeName) ->
              Node = maps:get(NodeName, NodeMap),
              net_adm:ping(Node) == pong
      end, Nodes).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
