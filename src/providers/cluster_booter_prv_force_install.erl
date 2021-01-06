%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_force_install).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, force_install).
-define(DEPS, [packages]).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "show erlang application installed at target host."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.


do(State) ->
    case cluster_booter_file_lib:install(State, true) of
        {ok, State1} ->
            cluster_booter_prv_config:do(State1);
        {error, Reason} ->
            {error, Reason}
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
