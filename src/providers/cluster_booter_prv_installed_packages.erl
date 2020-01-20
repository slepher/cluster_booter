%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_installed_packages).

%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, installed).
-define(DEPS, []).



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
    AllInOne = cluster_booter_state:all_in_one(State),
    Root = cluster_booter_state:root(State),
    io:format("root is ~p~n", [Root]),
    Hosts = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_state:current_host(State),
    InstalledPackages = 
        maps:fold(
          fun(Host, Nodes, Acc) ->
                  Opts = [{host, Host}, {current_host, CurrentHost}],
                  HostSt = 
                      lists:foldl(
                        fun(Node, Acc1) ->
                                NodeDir = 
                                    case AllInOne of
                                        false ->
                                            filename:join([Root]);
                                        _ ->
                                            filename:join([Root, "clients"])
                                    end,
                                Cmd = cluster_booter_cmd:cmd(exists, [{base_dir, NodeDir}, {node_name, Node}], Opts),
                                Value = os:cmd(Cmd),
                                case Value of
                                    "ok\n" ->
                                        maps:put(Node, true, Acc1);
                                    _ ->
                                        maps:put(Node, false, Acc1)
                                end
                        end, maps:new(), Nodes),
                  maps:put(Host, HostSt, Acc)
          end, maps:new(),Hosts),
    cluster_booter_packages:format_installed(InstalledPackages),
    NState = cluster_booter_state:installed_packages(State, InstalledPackages),
    {ok, NState}.
    
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
