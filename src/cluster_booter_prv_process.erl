%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_process).

-export([init/1, do/1, format_error/1]).

%% API
-define(PROVIDER, process_status).
-define(DEPS, []).

%%%===================================================================
%%% API
%%%===================================================================
-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "show erlang processes started no other hosts."}
                                ]),
    State1 = cluster_booter_state:add_provider(State, Provider),
    {ok, State1}.

do(State) ->
    HostMap = cluster_booter_state:hosts(State),
    CurrentHost = cluster_booter_status:current_host(State),
    Hosts = maps:keys(HostMap),
    RunningProcesses = 
        lists:foldl(
          fun(Host, Acc) ->
                  Processes = processes(Host, CurrentHost),
                  lists:foldl(
                    fun(Process, Acc1) ->
                            [[{host, Host}|Process]|Acc1]
                    end, Acc, Processes)
          end, [], Hosts),
    print_running_processes(RunningProcesses),
    NState = cluster_booter_state:running_processes(State, RunningProcesses),
    {ok, NState}.

format_error(_Error) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
processes(Host, CurrentHost) ->
    Cmd = cluster_booter_cmd:cmd(processes, [], [{host, Host}, {current_host, CurrentHost}]),
    ErlangProcesses = string:split(os:cmd(Cmd), "\n", all),
    lists:foldl(
      fun(ErlangProcess, Acc) ->
              case parse_erlang_process(string:split(ErlangProcess, " ", all), []) of
                  [] ->
                      Acc;
                  ProcessInfo ->
                      [ProcessInfo|Acc]
              end
      end, [], ErlangProcesses).

parse_erlang_process(["-name",Name|Rest], Acc) ->
    parse_erlang_process(Rest, [{name, Name}|Acc]);
parse_erlang_process(["-setcookie",Cookie|Rest], Acc) ->
    parse_erlang_process(Rest, [{cookie, Cookie}|Acc]);
parse_erlang_process([_Head|Rest], Acc) ->
    parse_erlang_process(Rest, Acc);
parse_erlang_process([], Acc) ->
    Acc.

print_running_processes(RunningProcesses) ->
    lists:foreach(
      fun(RunningProcess) ->
              Host = proplists:get_value(host, RunningProcess, ""),
              Name = proplists:get_value(name, RunningProcess, ""),
              Cookie = proplists:get_value(cookie, RunningProcess, ""),
              io:format("node ~s with cookie ~s running on ~s~n", [Name, Cookie, Host])
      end, RunningProcesses).
