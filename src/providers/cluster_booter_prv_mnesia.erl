%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_mnesia).

-include_lib("astranaut/include/macro.hrl").

-use_macro({rpc_function, transform/1, [{group_args, true}]}).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, mnesia).
-define(DEPS, [start_nodes]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "install mnesia definded by mnescha_schema."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    SchemaModule = cluster_booter_state:mnesia_schema(State),
    MnesiaNodeMap = cluster_booter_state:mnesia_nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    Result = 
        maps:fold(
          fun(NodeName, MnesiaNodes, Acc) ->
                  case cluster_booter_mnesia:initialize(MnesiaNodes, NodeMap, SchemaModule) of
                      ok ->
                          Acc;
                      {error, Reason} ->
                          maps:put(NodeName, Reason, Acc)
                  end
          end, maps:new(), MnesiaNodeMap),
    case maps:size(Result) of
        0 ->
            import_from_dump_if(State);
        _ ->
            {error, Result}
    end.

format_error({nodes_unready, Nodes}) when is_list(Nodes) ->
    NodesStr = string:join(lists:map(fun(Node) -> atom_to_list(Node) end, Nodes), ","),
    io_lib:format("Nodes not ready ~s", [NodesStr]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
import_from_dump_if(State) ->
    case cluster_booter_state:data(State) of
        "" ->
            {ok, State};
        DumpFile ->
            case file:consult(DumpFile) of
                {ok, DumpData} ->
                    init_nodes(DumpData, State);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

init_nodes([], State) ->
    {ok, State};
init_nodes([{NodeName, DumpData}|T], State) ->
    MasterNode = cluster_booter_state:get_node(NodeName, State),
    Groups = proplists:get_value(groups, DumpData, []),
    Groups0 = cluster_booter_state:import_groups(State),
    State1 = cluster_booter_state:import_groups(State, Groups ++ Groups0),
    Datas = proplists:get_value(records, DumpData, []),
    case init_tables(MasterNode, Datas) of
        ok ->
            init_nodes(T, State1);
        {error, Reason} ->
            {error, Reason}
    end.

init_tables(_MasterNode, []) ->
    ok;
init_tables(MasterNode, [{Table, Datas}|T]) ->
    case init_datas(MasterNode, Table, Datas) of
        ok ->
            init_tables(MasterNode, T);
        {error, Reason} ->
            {error, {import_table_data_failed, Table, Reason}}
    end.


init_datas(MasterNode, Table, Datas) ->
    io:format("write records ~p~n", [Table]),
    F = rpc_function:transform(
          fun () ->
                  lists:foreach(
                    fun(Data) ->
                            mnesia:write(Table, Data, write)
                    end, Datas)
          end, Datas, Table),
    case rpc:call(MasterNode, mnesia, transaction, [F]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.
