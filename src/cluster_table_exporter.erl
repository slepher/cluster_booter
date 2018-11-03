%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_table_exporter).

%% API
-export([tables/0, format/1]).

%%%===================================================================
%%% API
%%%===================================================================

tables() ->
    Tables = mnesia:system_info(tables),
    lists:foldl(
      fun(Table, NodeTableMap) ->
              DiscNodes = mnesia:table_info(Table, disc_copies),
              Attributes = mnesia:table_info(Table, attributes),
              Indexes = mnesia:table_info(Table, index),
              RecordName = mnesia:table_info(Table, record_name),
              AtomIndexes = lists:map(
                              fun(Index) ->
                                      lists:nth(Index - 1, Attributes)
                              end, Indexes),
              Info = #{name => RecordName},
              NInfo = 
                  case AtomIndexes of
                      [] ->
                          Info;
                      _ ->
                          maps:put(indexes, AtomIndexes, Info)
                  end,
              NNInfo = 
                  case Table of
                      RecordName ->
                          NInfo;
                      _ ->
                          maps:put(table, Table, NInfo)
                  end,
              NNodeTableMap = 
                  lists:foldl(
                    fun(Node, Acc) ->
                            NodeTables = maps:get(Node, Acc, []),
                            maps:put(Node, [NNInfo|NodeTables], Acc)
                    end, NodeTableMap, DiscNodes),
             NNodeTableMap
      end, maps:new(), Tables).

format(NodeTableMap) ->
    FormatPart = 
        fun(Map, Key) ->
                case maps:get(Key, Map, undefined) of
                    undefined ->
                        [];
                    Value ->
                        [lists:flatten(io_lib:format("~p = ~p", [Key, Value]))]
                end
        end, 
    maps:fold(
      fun(Node, Ts, _) ->
              Tables = Ts -- [#{name => schema}],
              case Tables of
                  [] ->
                      ok;
                  _ ->
                      io:format("      ~p => ~n", [Node]),
                      io:format("        mnesia_schema(~n"),
                      NTables = 
                          lists:sort(
                            fun(Table1, Table2) ->
                                    maps:get(name, Table1) < maps:get(name, Table2)
                            end, Tables),
                      TableStrs = 
                          lists:map(
                            fun(Table) ->
                                    lists:flatten(
                                      case maps:size(Table) of
                                          1 ->
                                              io_lib:format("          ~p", [maps:get(name, Table)]);
                                          _ ->
                                              io_lib:format("          #~p{", [maps:get(name, Table)]) ++
                                                  io_lib:format("~s", [string:join(FormatPart(Table, table) ++ FormatPart(Table, indexes), ",")]) ++ 
                                                  io_lib:format("}", [])
                                      end)
                            end, NTables),
                      io:format(string:join(TableStrs, ",~n")),
                      io:format("~n"),
                      io:format("        ),~n")
              end
      end, ok, NodeTableMap).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
