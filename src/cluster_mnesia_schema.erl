%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_mnesia_schema).

-export([groups_tables/2]).
-export([tables/3, node_groups/3, table_nodes/2]).
-export([table_master_node/2]).

-callback(applications() -> #{atom() := [atom()]}).
-callback(tables() -> #{atom() := [#{name := atom(), fields := [atom()], table => atom(), indexes => [atom()]}]}).

groups_tables([all], GroupDefs) ->
    groups_tables(maps:keys(GroupDefs), GroupDefs);
groups_tables(Groups, GroupDefs) ->
    groups_tables(Groups, GroupDefs, [], []).

groups_tables([], _GroupDefs, TablesAcc, IncrementsAcc) ->
    {ok, {TablesAcc, IncrementsAcc}};
groups_tables([Group|T], GroupDefs, TablesAcc, IncrementsAcc) ->
    case group_defs(Group, GroupDefs) of
        {ok, {Tables, Increments}} ->
            IncrementsAcc1 = 
                lists:foldl(
                  fun({IncrementTable, IncrementRecords}, Acc) ->
                          orddict:append_list(IncrementTable, IncrementRecords, Acc)
                  end, IncrementsAcc, Increments),
            groups_tables(T, GroupDefs, Tables ++ TablesAcc, IncrementsAcc1);
        {error, Reason}  ->
            {error, Reason}
    end.

group_defs(Group, GroupDefs) ->
    case maps:find(Group, GroupDefs) of
        {ok, #{tables := Tables} = GroupDef} ->
            Increments = maps:get(increments, GroupDef, #{}),
            {ok, {Tables, Increments}};
        {ok, Tables} when is_list(Tables) ->
            {ok, {Tables, []}};
        error ->
            {error, {no_group, Group}}
    end.    

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
                      maps:put(Node, {NodeName, Options, lists:usort(NodeNameGroups ++ NodeGroups)}, Acc);
                  error ->
                      Acc
              end
      end, maps:new(), maps:with(NodeNames, NodeNameGroupsMap)).

table_nodes(NodeGroups, GroupTables) ->
    maps:fold(
      fun(Node, {NodeName, Options, Groups}, Acc0) ->
              lists:foldl(
                fun(Group0, Acc1) ->
                        {Group, GroupOptions} = group_with_options(Group0),
                        case maps:find(Group, GroupTables) of
                            {ok, Tables} ->
                                lists:foldl(
                                  fun(Table, Acc2) ->
                                          #{name := Name} = Table,
                                          Options1 = maps:merge(Options, Table),
                                          Options2 = maps:merge(GroupOptions, Options1),
                                          TableName = maps:get(table, Table, Name),
                                          AccTable = maps:get(TableName, Acc2, #{}),
                                          CopyKey = 
                                              case maps:get(ram_copies, Options2, false) of
                                                  false ->
                                                      disc_copies;
                                                  true ->
                                                      ram_copies
                                              end,
                                          NodeNames = maps:get(nodes, Acc2, []),

                                          NodeNames1 = 
                                              case CopyKey of
                                                  disc_copies ->
                                                      [NodeName|NodeNames];
                                                  _ ->
                                                      NodeNames
                                              end,
                                          TableNodes = maps:get(CopyKey, AccTable, []),
                                          NTableNodes = ordsets:add_element(Node, TableNodes),
                                          NAccTable = maps:merge(
                                                        Table, 
                                                        AccTable#{table => TableName, 
                                                                  CopyKey => NTableNodes,
                                                                  nodes => NodeNames1
                                                                 }),
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

group_with_options(Group) when is_atom(Group) ->
    {Group, #{}};
group_with_options({Group, Option}) when is_atom(Group) ->
    Option1 = cluster_booter_mnesia:format_options(Option),
    {Group, Option1}.

