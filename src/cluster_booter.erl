%%%-------------------------------------------------------------------
%%% @author Chenxy <cxy@issac.local>
%%% @copyright (C) 2018, Chenxy
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2018 by Chenxy <cxy@issac.local>
%%%-------------------------------------------------------------------
-module(cluster_booter).

-compile({parse_transform, do}).

%% API
-export([rpc_call/4]).
-export([main/1, format_error/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(Args) ->
    {ok, _} = application:ensure_all_started(erlando),
    case getopt:parse([], Args) of
        {ok, {Options, NonOptions}} ->
            case lists:member(help, Options) or (NonOptions == []) of
                true ->
                    usage();
                false ->
                    [Command|_] = NonOptions,
                    do(Options, [list_to_atom(Command)])
            end;
        {error, Detail} ->
            format_error(Detail)
    end.

do(Options, Actions) ->
    State = cluster_booter_state:new(),
    ConfigFile = cluster_booter_config_base:config_file(Options, config, "booter.config"),
    Result = 
        do([error_m ||
               Config <- cluster_booter_config_base:config(Options, ConfigFile),
               NState <- cluster_booter_state:load_terms(Config, State),
               cluster_booter_state:validate(NState),
               NNState <- cluster_booter_state:initialize(NState),
               set_node_name_and_cookie(NNState),
               ok = io:format("cookie is ~p, node is ~p.~n", [erlang:get_cookie(), node()]),
               AllProviders = cluster_booter_state:providers(NNState),
               cluster_booter_providers:run(Actions, AllProviders, NNState)
           ]),
    handle_output(State, command_line, Result).

set_node_name_and_cookie(State) ->
    NodeName = cluster_booter_state:node_name(State),
    Cookie = cluster_booter_state:cookie(State),
    net_kernel:start([NodeName, longnames]),
    erlang:set_cookie(node(), Cookie),
    ok.

handle_output(_State, command_line, E={error, _}) ->
    io:format("error ~p~n", [E]),
    init:stop(127);
handle_output(_State, command_line, _) ->
    init:stop(0);
handle_output(_State, api, Result) ->
    Result.

usage() ->
    io:format("ok"),
    ok.

-spec format_error(Reason::term()) -> string().
format_error({invalid_return_value, Provider, Value}) ->
    io_lib:format(lists:flatten([providers:format(Provider), " returned an invalid value ",
                                 io_lib:format("~p", [Value])]), []);
format_error({opt_parse, {invalid_option, Opt}}) ->
    io_lib:format("invalid option ~s~n", [Opt]);
format_error({opt_parse, Arg}) ->
    io_lib:format("~p~n", [Arg]);
format_error({invalid_action, Action}) ->
    io_lib:format("Invalid action specified: ~p~n", [Action]);
format_error({error, {relx, Reason}}) ->
    format_error(Reason);
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
