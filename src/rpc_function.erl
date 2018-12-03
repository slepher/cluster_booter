%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(rpc_function).

-compile({parse_transform, ast_quote}).

%% API
-export([transform/1, rpc_function/2]).

%%%===================================================================
%%% API
%%%===================================================================
transform([Function|Bindings]) ->
    AtomVarBindings = atom_var_bindings(Bindings, 0),
    FunctionAst = ast_quote:quote(Function, 0),
    quote(?MODULE:rpc_function(unquote(FunctionAst), unquote(AtomVarBindings))).

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
atom_var_bindings([H|T], _Line) ->
    {Line, Val} = atom_var_val(H),
    {cons, Line, {tuple, Line, [{atom, Line, Val}, {var, Line, Val}]}, atom_var_bindings(T, Line)};
atom_var_bindings([], Line) ->
    {nil, Line}.

atom_var_val({atom, Line, Val}) ->
    {Line, Val};
atom_var_val({var, Line, Val}) ->
    {Line, Val}.
