%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rpc_function_transform).

-compile({parse_transform, ast_quote}).

%% API
-export([parse_transform/2, format_error/1]).
-export([rpc_function/2]).

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

rpc_function(FunctionAst, AtomVarBindings) ->
    BindingValues = 
        lists:foldl(
          fun({Atom, Var}, Acc) ->
                  erl_eval:add_binding(Atom, Var, Acc)
          end, erl_eval:new_bindings(), AtomVarBindings),
    {value, Fun, _} = erl_eval:expr(FunctionAst, BindingValues),
    Fun.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre, {call, Line1, {atom, Line2, rpc_fun}, [Function|Bindings]}) ->
    AtomVarBindings = atom_var_bindings(Bindings, Line2),
    FunctionAst = ast_quote:quote(Function, Line1),
    quote(
      ?MODULE:rpc_function(unquote(FunctionAst), unquote(AtomVarBindings)),
      Line1);
walk(_Type, Node) ->
    Node.

atom_var_bindings([H|T], _Line) ->
    {Line, Val} = atom_var_val(H),
    {cons, Line, {tuple, Line, [{atom, Line, Val}, {var, Line, Val}]}, atom_var_bindings(T, Line)};
atom_var_bindings([], Line) ->
    {nil, Line}.

atom_var_val({atom, Line, Val}) ->
    {Line, Val};
atom_var_val({var, Line, Val}) ->
    {Line, Val}.

%variables(FunctionForm) ->
%    ast_traverse:reduce(fun walk_variables/3, {[], []}, FunctionForm).

%walk_variables({pre, {clause, Line, Patterns, _Guards, Expressions}, {_Status, Variables, 
              
