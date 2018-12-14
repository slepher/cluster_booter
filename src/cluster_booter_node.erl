%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_booter_node).

-compile({parse_transform, make_lenses}).

%% API
-export([new_status/0]).
-export([check/2, started/2, stopped/2]).
-export([node/2, wait/3]).
-export([print/1]).

-record(status, {started_nodes = [],
                 stopped_nodes = [],
                 undefined_nodes = [],
                 node_map = maps:new()
                }).


-make_lenses([status]).

new_status() ->
    #status{}.
%%%===================================================================
%%% API
%%%===================================================================
wait(NodeStatus, Timeout, Status) when Timeout < 0 ->
    RestNodes = rest_nodes(NodeStatus, Status),
    {error, {wait_timeout, RestNodes}};
wait(NodeStatus, Timeout, Status) ->
    NodeMap = node_map(NodeStatus),
    case rest_nodes(NodeStatus, Status) of
        [] ->
            {ok, NodeStatus};
        RestNodes ->
            timer:sleep(200),
            NNodeStatus = check(RestNodes, NodeMap),
            NNNodeStatus = merge_status(NodeStatus, NNodeStatus, Status),
            wait(NNNodeStatus, Timeout - 200, Status)
    end.

started(Node, #status{started_nodes = StartedNodes}) ->
    lists:member(Node, StartedNodes).

stopped(Node, #status{stopped_nodes = StoppedNodes}) ->
    lists:member(Node, StoppedNodes).

merge_status(#status{started_nodes = StartedNodes} = Status,
             #status{started_nodes = NStartedNodes, stopped_nodes = NStoppedNodes}, started) ->
    Status#status{started_nodes = StartedNodes ++ NStartedNodes, 
                  stopped_nodes = NStoppedNodes};
merge_status(#status{stopped_nodes = StoppedNodes} = Status,
             #status{started_nodes = NStartedNodes, stopped_nodes = NStoppedNodes}, stopped) ->
    Status#status{started_nodes = NStartedNodes,
                  stopped_nodes = StoppedNodes ++ NStoppedNodes}.

check(NodeNames, NodeMap) ->
    NodeStatus = new_status(),
    lists:foldl(
      fun(NodeName, StateAcc) ->
              case check_node(NodeName, NodeMap) of
                  started ->
                      add_started_node(NodeName, StateAcc);
                  stopped ->
                      add_stopped_node(NodeName, StateAcc);
                  undefined ->
                      add_undefined_node(NodeName, StateAcc)
              end
      end, NodeStatus#status{node_map = NodeMap}, NodeNames).

check_node(NodeName, NodeMap) ->
    case maps:find(NodeName, NodeMap) of
        {ok, Node} ->
            case net_adm:ping(Node) of
                pong ->
                    started;
                pang ->
                    stopped
            end;
        error ->
            undefined
    end.

node(NodeName, Host) ->
    binary_to_atom(list_to_binary([atom_to_list(NodeName), "@", Host]), utf8).

print(#status{started_nodes = StartedNodes, stopped_nodes = StoppedNodes, undefined_nodes = UndefinedNodes}) ->
    print_nodes(StartedNodes, started),
    print_nodes(StoppedNodes, stopped),
    print_nodes(UndefinedNodes, undefined).

print_nodes([], _) ->
    ok;
print_nodes(Nodes, Status) ->
    io:format("~p nodes:\n", [Status]),
    lists:foreach(
      fun(Node) ->
              io:format("  ~p~n", [Node])
      end, Nodes).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
rest_nodes(#status{stopped_nodes = Stopped}, started) ->
    Stopped;
rest_nodes(#status{started_nodes = Started}, stopped) ->
    Started.

add_started_node(Node, #status{started_nodes = Nodes} = NodeStatus) ->
    NodeStatus#status{started_nodes = [Node|Nodes]}.

add_stopped_node(Node, #status{stopped_nodes = Nodes} = NodeStatus) ->
    NodeStatus#status{stopped_nodes = [Node|Nodes]}.

add_undefined_node(Node, #status{undefined_nodes = Nodes} = NodeStatus) ->
    NodeStatus#status{undefined_nodes = [Node|Nodes]}.
