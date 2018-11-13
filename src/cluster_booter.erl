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
-export([rpc_call/4, return_mnesia_rpc/2]).
-export([main/1, format_error/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(Args) ->
    {ok, _} = application:ensure_all_started(erlando),
    case getopt:parse([], Args) of
        {ok, {Options, NonOptions}} ->
            do(Options, NonOptions);
        {error, Detail} ->
            format_error(Detail)
    end.

do(Options, NonOptions) ->
    State = cluster_booter_state:new(),
    ConfigFile = cluster_booter_config_base:config_file(Options, config, "booter.config"),
    Result = 
        do([error_m ||
               Config <- cluster_booter_config_base:config(Options, ConfigFile),
               NState <- cluster_booter_state:load_terms(Config, State),
               cluster_booter_state:validate(NState),
               NNState <- cluster_booter_state:initialize(NState),
               set_node_name_and_cookie(NNState),
               AllProviders = cluster_booter_state:providers(NNState),
               case lists:member(help, Options) or (NonOptions == []) of
                   true ->
                       usage(AllProviders);
                   false ->
                       [Command|_] = NonOptions,
                       Actions = [list_to_atom(Command)],
                       cluster_booter_providers:run(Actions, AllProviders, NNState)
               end
           ]),
    handle_output(State, command_line, Result).

set_node_name_and_cookie(State) ->
    NodeName = cluster_booter_state:node_name(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Node = list_to_atom(atom_to_list(NodeName) ++ "@" ++ CurrentHost),
    Cookie = cluster_booter_state:cookie(State),
    case net_adm:names() of
        {ok, _} -> %% Epmd is running
            ok;
        {error, address} ->
            Epmd = os:find_executable("epmd"),
            os:cmd(Epmd ++ " -daemon")
    end,
    net_kernel:start([Node, longnames]),
    erlang:set_cookie(node(), Cookie),
    ok.

handle_output(_State, command_line, E={error, _}) ->
    io:format("error ~p~n", [E]),
    init:stop(127);
handle_output(_State, command_line, _) ->
    init:stop(0);
handle_output(_State, api, Result) ->
    Result.

usage(AllProviders) ->
    io:format("cluster_booter [command]:~n"),
    lists:foreach(
      fun(Provider) ->
              Name = providers:impl(Provider),
              Desc = case providers:desc(Provider) of
                         undefined ->
                             "";
                         Val ->
                             Val
                     end,
              io:format("~4s~-20s~s~n", ["", Name, Desc])
      end, AllProviders).

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
            {ok, Other}
    end.

return_mnesia_rpc(Node, Value) ->
    case Value of
        {atomic, Val} ->
            {ok, Val};
        {aborted, Reason} ->
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
