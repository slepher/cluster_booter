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
-callback(tables() -> #{atom() := [#{name := atom(), fields := [atom()], table => atom(), indexes => [atom()]}]}).


tables(NodeWithOptions, NodeMap, Module) ->
    NodeNameGroupsMap = Module:applications(),
    GroupTables = Module:tables(),
    NodeGroups = node_groups(NodeWithOptions, NodeNameGroupsMap, NodeMap),
    table_nodes(NodeGroups, GroupTables).

node_groups(NodeNameWithOpts, NodeNameGroupsMap, NodeMap) ->
    NodeNames = lists:map(fun({NodeName, _Options}) -> NodeName end, NodeNameWithOpts),
    NameOptsMap = maps:from_list(NodeNameWithOpts),
    maps:fold(
      fun(NodeName, NodeNameGroups, Acc) ->
              case maps:find(NodeName, NodeMap) of
                  {ok, Node} ->
                      Options = maps:get(NodeName, NameOptsMap, #{}),
                      NodeGroups = maps:get(Node, Acc, []),
                      maps:put(Node, {Options, lists:usort(NodeNameGroups ++ NodeGroups)}, Acc);
                  error ->
                      Acc
              end
      end, maps:new(), maps:with(NodeNames, NodeNameGroupsMap)).

table_nodes(NodeGroups, GroupTables) ->
    maps:fold(
      fun(Node, {Options, Groups}, Acc0) ->
              lists:foldl(
                fun(Group, Acc1) ->
                        case maps:find(Group, GroupTables) of
                            {ok, Tables} ->
                                lists:foldl(
                                  fun(Table, Acc2) ->
                                          #{name := Name} = Table,
                                          Options1 = maps:merge(Options, Table),
                                          TableName = maps:get(table, Table, Name),
                                          AccTable = maps:get(TableName, Acc2, #{}),
                                          CopyKey = 
                                              case maps:get(ram_copies, Options1, false) of
                                                  false ->
                                                      disc_copies;
                                                  true ->
                                                      ram_copies
                                              end,
                                          TableNodes = maps:get(CopyKey, AccTable, []),
                                          NTableNodes = ordsets:add_element(Node, TableNodes),
                                          NAccTable = maps:merge(Table, AccTable#{table => TableName, CopyKey => NTableNodes}),
                                          maps:put(TableName, NAccTable, Acc2)
                                  end, Acc1, Tables);
                            error ->
                                exit({group_not_exits, Group})
                        end
                end, Acc0, Groups)
      end, maps:new(), NodeGroups).

table_master_node(SchemaMasterNode, Table) ->
    case rpc:call(SchemaMasterNode, mnesia, table_info, [Table, disc_copies]) of
        [Node|_T] ->
            {ok, Node};
        [] ->
            {error, no_table}
    end.
