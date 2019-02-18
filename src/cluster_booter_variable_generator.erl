%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_variable_generator).

%% API
-export([variables/2]).

%%%===================================================================
%%% API
%%%===================================================================
variables(Env, Opts) ->
    lists:foldl(
      fun(VaiableOpt, Acc) ->
              VariableOpt1 = to_map(VaiableOpt),
              Key = maps:get(key, VariableOpt1),
              Value = proplists:get_value(Key, Env),
              ValuePairs = to_value(Key, Value),
              maps:merge(ValuePairs, Acc)
      end, maps:new(), Opts).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
to_map(Atom) when is_atom(Atom) ->
    #{key => Atom};
to_map(Proplists) when is_list(Proplists) ->
    lists:foldl(
      fun(Key, Acc) when is_atom(Key) ->
              maps:put(Key, true, Acc);
         ({Key, Value}, Acc) ->
              maps:put(Key, Value, Acc)
      end, maps:new(), Proplists);
to_map(Map) when is_map(Map) ->
    Map.

to_value(Key, Value) when is_map(Value) ->
    maps:fold(
      fun(MapKey, MapValue, MapAcc) ->
              NKey = list_to_atom(lists:flatten(io_lib:format("~s_~s", [atom_to_list(Key), atom_to_list(MapKey)]))),
              Value1 = to_value(NKey, MapValue),
              maps:merge(Value1, MapAcc)
      end, maps:new(), Value);
to_value(Key, Value) when is_list(Value) ->
    case lists:any(fun(ListValue) -> is_integer(ListValue) end, Value) of
        true ->
            #{Key => Value};
        false ->
            MapValue = to_map(Value),
            to_value(Key, MapValue)
    end;
to_value(Key, Value) ->
    #{Key => Value}.



    
