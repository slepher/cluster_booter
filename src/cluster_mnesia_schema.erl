%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_mnesia_schema).

-export([tables/3, node_groups/3, table_nodes/2]).
-export([table_master_node/2]).

-callback(applications() -> #{atom() := [atom()]}).
-callback(tables() -> #{atom() := #{name := atom(), table => atom(), indexes => [atom()]}}).


tables(NodeNames, NodeMap, Module) ->
    NodeNameGroupsMap = Module:applications(),
    GroupTables = Module:tables(),
    NodeGroups = node_groups(NodeNames, NodeNameGroupsMap, NodeMap),
    table_nodes(NodeGroups, GroupTables).

node_groups(NodeNames, NodeNameGroupsMap, NodeMap) ->
    maps:fold(
      fun(NodeName, NodeNameGroups, Acc) ->
              case maps:find(NodeName, NodeMap) of
                  {ok, Node} ->
                      NodeGroups = maps:get(Node, Acc, []),
                      maps:put(Node, lists:usort(NodeNameGroups ++ NodeGroups), Acc);
                  error ->
                      Acc
              end
      end, maps:new(), maps:with(NodeNames, NodeNameGroupsMap)).

table_nodes(NodeGroups, GroupTables) ->
    maps:fold(
      fun(Node, Groups, Acc0) ->
              lists:foldl(
                fun(Group, Acc1) ->
                        case maps:find(Group, GroupTables) of
                            {ok, Tables} ->
                                lists:foldl(
                                  fun(Table, Acc2) ->
                                          #{name := Name} = Table,
                                          TableNodes = 
                                              case maps:find(Name, Acc2) of
                                                  {ok, #{nodes := Value}} ->
                                                      Value;
                                                  error ->
                                                      []
                                              end,
                                          TableName = maps:get(table, Table, Name),
                                          NTable = maps:put(table, TableName, Table),
                                          NNTable = maps:put(nodes, ordsets:add_element(Node, TableNodes), NTable),
                                          maps:put(TableName, NNTable, Acc2)
                                  end, Acc1, Tables);
                            error ->
                                exit({group_not_exits, Group})
                        end
                end, Acc0, Groups)
      end, maps:new(), NodeGroups).

table_master_node(SchemaMasterNode, Table) ->
    case rpc:call(SchemaMasterNode, mnesia, table_info, [Table, disc_copies]) of
        [Node|T] ->
            {ok, Node};
        [] ->
            {error, no_table}
    end.
