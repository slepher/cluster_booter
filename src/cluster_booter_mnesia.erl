%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_booter_mnesia).

-include_lib("astranaut/include/macro.hrl").

-use_macro({rpc_function, transform/1, [{group_args, true}]}).

%% API
-export([validate_mnesia_clusters/2, validate_mnesia_cluster/2]).
-export([format_options/1]).

-export([initialize/3,
         create_schema/2,
         create_tables/2,
         init_datas/3,
         drop_tables/1
        ]).
-export([validate_nodes/0, validate_nodes/1, boot_mnesia/2]).
-export([cluster_nodes/2, master_node/1, tables/1, validate_schema/1,
         cookie_tables_map/1]).

%%%===================================================================
%%% API
%%%===================================================================

validate_mnesia_clusters(MnesiaClusters, NodeMap) ->
    maps:fold(
      fun(ClusterName, NodeNames, Acc) ->
              NodeWithOptions = update_nodes(NodeNames),
              case validate_mnesia_cluster(NodeWithOptions, NodeMap) of
                  ok ->
                      Acc;
                  {error, Reason} ->
                      maps:put(ClusterName, Reason, Acc)
              end
      end, maps:new(), MnesiaClusters).

update_nodes(NodeNames) ->
    lists:map(
      fun(NodeName) when is_atom(NodeName) ->
              {NodeName, #{}};
         ({NodeName, Options}) ->
              {NodeName, format_options(Options)}
      end, NodeNames).

format_options(Option) when is_atom(Option) ->
    #{Option => true};
format_options(Options) when is_list(Options) ->
    lists:foldl(
      fun(Key, Acc) when is_atom(Key) ->
              Acc#{Key => true};
         ({Key, Value}, Acc) ->
              Acc#{Key => Value}
      end, #{}, Options);
format_options(#{} = Options) ->
    Options.

disc_copy_nodes(NodeWithOptions) ->
    lists:foldl(
      fun({Node, Options}, Acc) ->
              case maps:get(ram_copies, Options, false) of
                  false ->
                      [Node|Acc];
                  true ->
                      Acc
              end
      end, [], NodeWithOptions).

ram_copy_nodes(NodeWithOptions) ->
    lists:foldl(
      fun({Node, Options}, Acc) ->
              case maps:get(ram_copies, Options, false) of
                  true ->
                      [Node|Acc];
                  false ->
                      Acc
              end
      end, [], NodeWithOptions).

validate_mnesia_cluster(NodeNames, NodeMap) ->
    DiscCopyNodeNames = disc_copy_nodes(NodeNames),
    Nodes = lists:usort(maps:values(maps:with(DiscCopyNodeNames, NodeMap))),
    case master_node(Nodes) of
        {ok, MasterNode} ->
            WorkingNodes = rpc:call(MasterNode, mnesia, table_info, [schema, disc_copies]),
            case Nodes -- WorkingNodes of
                [] ->
                    ok;
                Unbooted ->
                    {error, {mnesia_uninitialized_nodes, Unbooted}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

create_schema(NodeWithOptions, NodeMap) ->
    DiscCopyNodeNames = disc_copy_nodes(NodeWithOptions),
    RamCopyNodeNames = ram_copy_nodes(NodeWithOptions),
    DiscCopyNodes = lists:usort(maps:values(maps:with(DiscCopyNodeNames, NodeMap))),
    RamCopyNodes = lists:usort(maps:values(maps:with(RamCopyNodeNames, NodeMap))),
    Nodes = DiscCopyNodes ++ RamCopyNodes,
    case master_node(DiscCopyNodes) of
        {ok, MasterNode} ->
            DiscCopies = rpc:call(MasterNode, mnesia, table_info, [schema, disc_copies]),
            RamCopies = rpc:call(MasterNode, mnesia, table_info, [schema, ram_copies]),
            RestCopies =  Nodes -- DiscCopies -- RamCopies,
            RestDiscCopies = DiscCopyNodes -- DiscCopies, 
            rpc:call(MasterNode, mnesia, change_config, [extra_db_nodes, RestCopies]),
            lists:foreach(
              fun(RamCopy) ->
                      rpc:call(MasterNode, mnesia, change_table_copy_type, [schema, RamCopy, disc_copies])
              end, RestDiscCopies),
            {ok, MasterNode};
        {error, Reason} ->
            {error, Reason}
    end.

validate_nodes() ->
    {ok, Nodes} = application:get_env(cluster_booter, nodes),
    AllNodes = all_nodes(Nodes),
    validate_nodes(AllNodes).

validate_nodes(Nodes) ->
    lists:filter(
      fun(Node) ->
              net_adm:ping(Node) /= pong
      end, Nodes).

initialize(NodeWithOptions0, NodeMap, Module) ->
    NodeWithOptions = update_nodes(NodeWithOptions0),
    DiscCopyNodeNames = disc_copy_nodes(NodeWithOptions),
    case boot_mnesia(NodeWithOptions, NodeMap) of
        {ok, ok} ->
            Nodes = lists:usort(maps:values(maps:with(DiscCopyNodeNames, NodeMap))),
            case Nodes of
                [] ->
                    ok;
                _ ->
                    case create_schema(NodeWithOptions, NodeMap) of
                        {ok, MasterNode} ->
                            Tables = cluster_mnesia_schema:tables(NodeWithOptions, NodeMap, Module),
                            CreateTableFails = create_tables(MasterNode, Tables),
                            case maps:size(CreateTableFails) of
                                0 ->
                                    ok;
                                _ ->
                                    {error, {create_table_failed, CreateTableFails}}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

boot_mnesia(NodeWithOptions0, NodeMap) ->
    NodeWithOptions = update_nodes(NodeWithOptions0),
    NodeNames = lists:map(fun({NodeName, _Options}) -> NodeName end, NodeWithOptions),
    NodeNames1 = lists:filter(fun(NodeName) -> maps:is_key(NodeName, NodeMap) end, NodeNames),
    NodeApplications = 
        lists:foldl(
          fun(Name, Acc) -> 
                  maps:put(Name, [mnesia], Acc)
          end, maps:new(), NodeNames1),
    case cluster_booter_application:node_applications(NodeNames1, NodeMap) of
        {ok, NodeApplicationsSt} ->
            ApplicationSt = cluster_booter_application:application_st(NodeApplications, NodeApplicationsSt),
            cluster_booter_application:boot_applications(ApplicationSt, NodeMap);
        {error, Reason} ->
            {error, Reason}
    end.

drop_tables(MasterNode) ->
    Tables = tables(MasterNode),
    lists:foreach(
      fun(Table) ->
              DiscCopies = rpc:call(MasterNode, mnesia, table_info, [Table, disc_copies]),
              lists:foreach(
                fun(DiscCopy) ->
                        rpc:call(MasterNode, mnesia, del_table_copy, [Table, DiscCopy])
                end, DiscCopies)
      end, Tables).

create_tables(MasterNode, Tables) ->
    CurrentTables = tables(MasterNode),
    maps:fold(
      fun(TableName, Table, Acc) ->
              case update_table_schema(MasterNode, CurrentTables, Table) of
                  ok ->
                      {atomic, ok} = rpc:call(MasterNode, mnesia, clear_table, [TableName]),
                      InitDatas = maps:get(init_data, Table, []),
                      case init_datas(MasterNode, TableName, InitDatas) of
                          ok ->
                              Acc;
                          {error, Reason} ->
                              maps:put(TableName, Reason, Acc)
                      end;
                  {error, Reason} ->
                      maps:put(TableName, Reason, Acc)
              end
      end, maps:new(), Tables).

update_table_schema(MasterNode, CurrentTables, #{table := TableName} = Table) ->
    case lists:member(TableName, CurrentTables) of
        true ->
            DiscCopies = rpc:call(MasterNode, mnesia, table_info, [TableName, disc_copies]),
            RamCopies = rpc:call(MasterNode, mnesia, table_info, [TableName, ram_copies]),
            del_table_copies(MasterNode, TableName, DiscCopies ++ RamCopies),
            create_table(MasterNode, Table);
        false ->
            create_table(MasterNode, Table)
    end.

del_table_copies(_MasterNode, _TableName, []) ->
    ok;
del_table_copies(MasterNode, TableName, TableNodes) ->
    rpc:call(MasterNode, mnesia, wait_for_tables, [[TableName], 3000]),
    lists:foreach(
      fun(Node) ->
              {atomic, ok} = rpc:call(MasterNode, mnesia, del_table_copy, [TableName, Node])
      end, TableNodes).

create_table(MasterNode, #{table := TableName, fields := Fields,
                           name := Name} = Table) ->
    TableDiscCopies = maps:get(disc_copies, Table, []),
    TableRamCopies = maps:get(ram_copies, Table, []),
    Indexes = maps:get(indexes, Table, []),
    TableType = maps:get(type, Table, set),
    case rpc:call(MasterNode, mnesia, create_table, 
                  [TableName, [{attributes, Fields}, {record_name, Name}, 
                               {disc_copies, TableDiscCopies}, {ram_copies, TableRamCopies}, {type, TableType}]]) of
        {atomic, ok} ->
            lists:foreach(
              fun(Index) ->
                      {atomic, ok} = rpc:call(MasterNode, mnesia, add_table_index, [TableName, Index])
              end, Indexes);
        {aborted, Reason} ->
            {error, Reason}
    end.
    
init_datas(MasterNode, Table, Datas) ->
    case Datas of
        [] ->
            ok;
        _ ->
            case cluster_mnesia_schema:table_master_node(MasterNode, Table) of
                {ok, Node} ->
                    F = rpc_function:transform(
                          fun () ->
                                lists:foreach(
                                  fun(Data) ->
                                          mnesia:write(Table, Data, write)
                                  end, Datas)
                          end, Datas, Table),
                    case rpc:call(Node, mnesia, transaction, [F]) of
                        {atomic, ok} ->
                            ok;
                        {aborted, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.
       
cluster_nodes(Clusters, Nodes) ->
    case Clusters of
        all ->
            #{main => all_nodes(Nodes)};
        _ ->
            maps:from_list(
              lists:map(
              fun({Name, NodeNames}) ->
                      {Name, lists:foldl(
                        fun(NodeName, Acc) ->
                                case proplists:get_value(NodeName, Nodes) of
                                    undefined ->
                                        exit({invalid_node, NodeName});
                                    Node ->
                                        [Node|Acc]
                                end
                        end, [], NodeNames)}
              end, Clusters))
    end.

master_node(Nodes) ->
    case validate_schema(Nodes) of
        {ok, []} ->
            {ok, lists:nth(1, Nodes)};
        {ok, [Node|_]} ->
            {ok, Node};
        {error, Reason} ->
            {error, Reason}
    end.

validate_schema(Nodes) ->
    CookieTablesMap = cookie_tables_map(Nodes),
    WorkingCookies = 
        maps:filter(
          fun(_Cookie, #{tables := Tables}) ->
                  Tables /= []
          end, CookieTablesMap),
    case maps:size(WorkingCookies) of
        0 ->
            {ok, []};
        1 -> 
            [#{nodes := WorkingNodes}] = maps:values(WorkingCookies),
            {ok, WorkingNodes};
        _ ->
            {error, {multi_working_cookies, maps:values(WorkingCookies)}}
    end.

cookie_tables_map(Nodes) ->
    lists:foldl(
          fun(Node, Acc) ->
                  Cookie = rpc:call(Node, mnesia, table_info, [schema, cookie]),
                  Tables = tables(Node),
                  CookieInfo = maps:get(Cookie, Acc, #{}),
                  %CookieTables = maps:get(tables, CookieInfo, []),
                  CookieNodes = maps:get(nodes, CookieInfo, []),
                  NCookieInfo = #{tables => lists:usort(Tables ++ CookieNodes), nodes => [Node|CookieNodes]},
                  maps:put(Cookie, NCookieInfo, Acc)
          end, maps:new(), Nodes).

tables(Node) ->
    rpc_call(Node, mnesia, table_info, [schema, tables]) -- [schema].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
all_nodes(Nodes) ->
    lists:usort(
      lists:map(
        fun({_NodeName, Node}) ->
                Node
        end, Nodes)).


rpc_call(Node, Module, Function, Args) ->
    case rpc:call(Node, Module, Function, Args) of
        ok ->
            ok;
        {ok, Val} ->
            {ok, Val};
        {error, Reason} ->
            {error, Reason};
        {badrpc, nodedown} ->
            {error, nodedown};
        {badrpc, Reason} ->
            {error, {badrpc, Node, Reason}};
        Other ->
            Other
    end.
