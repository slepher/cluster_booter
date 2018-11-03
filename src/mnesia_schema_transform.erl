%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(mnesia_schema_transform).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    ast_traverse:map(fun walk/2, Forms).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre, {call, Line, {atom, _Line, mnesia_schema}, Tables}) ->
    NNode = lists:map(fun update_table/1, Tables),
    NNNode = list_node(NNode, Line),
    NNNode;
walk(_Type, Node) ->
    Node.

list_node([Node], Line) ->
    {cons, Line, Node, {nil, Line}};
list_node([H|T], Line) ->
    {cons, Line, H, list_node(T, Line)}.

update_table({atom, Line, Name}) ->
    atom_to_map(Name, Line);
update_table({record, Line, Name, Fields}) ->
    NFields = lists:map(fun record_field_to_map_field/1, Fields),
    {map, Line, [atom_map_field(name, Name, Line),record_info_fields(Name, Line)|NFields]}.

atom_to_map(Name, Line) ->
    {map,Line,
     [{map_field_assoc,Line,{atom,Line,name},{atom,Line,Name}},
      {map_field_assoc,Line,{atom,Line,table},{atom,Line,Name}},
      record_info_fields(Name, Line)]}.

atom_map_field(Key, Value, Line) ->
  {map_field_assoc, Line, {atom, Line, Key}, atom(Value, Line)}.

atom(Value, Line) ->
    {atom, Line, Value}.

record_field_to_map_field({record_field, Line, Key, Value}) ->
  {map_field_assoc, Line, Key, Value}.

record_info_fields(Record, Line) ->
  {map_field_assoc, Line,
   {atom,Line,fields},
   {call,Line,
    {atom,Line,record_info},
    [{atom,Line,fields},
     {atom,Line,Record}]
   }}.