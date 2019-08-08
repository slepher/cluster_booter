%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_terms).

%% API
-export([scan_binary/1, scan_string/1]).

%%%===================================================================
%%% API
%%%===================================================================
scan_binary(Bin) ->
    TermString = unicode:characters_to_list(Bin, utf8),
    scan_string(TermString).

scan_string(TermString) ->
    case erl_scan:string(TermString) of
        {ok, Tokens, _} ->
            case lists:foldl(fun break_tokens/2, {[], []}, Tokens) of
                {[], TokenGroups} ->
                    AbsForms = [A || {ok, A} <- lists:map(fun erl_parse:parse_exprs/1, TokenGroups)],
                    [V || {value, V, _} <- lists:map(fun eval_terms/1, AbsForms)];
                {Other, _} ->
                    {error, {unparsed_tokens, lists:reverse(Other)}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

break_tokens({dot, Line}, {Tokens, TokensGroup}) ->
    {[], [lists:reverse([{dot, Line}|Tokens])|TokensGroup]};
break_tokens(Token, {Tokens, TokensGroup}) ->
    {[Token | Tokens], TokensGroup}.

eval_terms(Abstract) ->
    erl_eval:exprs(Abstract, erl_eval:new_bindings()).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
