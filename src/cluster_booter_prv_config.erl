%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_config).
%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, config).
-define(DEPS, [versions]).

init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "initialize node config with template and variables."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    BaseDir = cluster_booter_state:root(State),
    CurrentHost = cluster_booter_state:current_host(State),
    SysConfig = cluster_booter_state:sys_config(State),
    VMArgs = cluster_booter_state:vm_args(State),
    NodeVersions = cluster_booter_state:node_versions(State),
    cluster_booter_state:fold_host_nodes(
      fun(Host, Release, Node, Acc) ->
              CmdOpts = [{host, Host}, {current_host, CurrentHost}],
              case maps:find(Node, NodeVersions) of
                  {ok, Version} ->
                      SysConfigTemplate = bbmustache:parse_file(SysConfig),
                      VMArgsTemplate = bbmustache:parse_file(VMArgs),
                      FVariables = update_node_variables(Release, Node, State),
                      SysConfigResult = bbmustache:compile(SysConfigTemplate, FVariables),
                      VMArgResult = bbmustache:compile(VMArgsTemplate, FVariables),
                      Dir = filename:join([BaseDir, Node, "releases", Version]),
                      write_file(Dir, "sys.config", SysConfigResult, CmdOpts),
                      write_file(Dir, "vm.args", VMArgResult, CmdOpts),
                      Acc;
                  error ->
                      io:format("get version of ~p failed~n", [Node]),
                      Acc
              end
      end, ok, State),
    {ok, State}.
    
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
%%%===================================================================
%%% Internal functions
%%%===================================================================
update_node_variables(Release, NodeName, State) ->
    Root = cluster_booter_state:root(State),
    MnesiaDir = cluster_booter_state:mnesia_dir(State),
    LogDir = cluster_booter_state:log_dir(State),

    Cookie = cluster_booter_state:cookie(State),
    NodeMap = cluster_booter_state:node_map(State),
    Variables = cluster_booter_state:variables(State),
    NodeVariablesMap = cluster_booter_state:node_variables(State),
    IsNode = list_to_atom(lists:flatten(io_lib:format("is_~p", [NodeName]))),
    IsRelease = list_to_atom(lists:flatten(io_lib:format("is_~p", [Release]))),
    Node = maps:get(NodeName, NodeMap),
    NodeInfo = #{IsNode => true, IsRelease => true, node_name => NodeName,
                 node => Node, release_name => Release, cookie => Cookie, root => Root,
                 mnesia_dir => MnesiaDir, log_dir => LogDir},
    NodeVariables = maps:get(Node, NodeVariablesMap, maps:new()),
    ReleaseVariables = maps:get(Release, NodeVariablesMap, maps:new()),
    VariableMap = 
        lists:foldl(
          fun(Info, Acc) ->
                  maps:merge(Info, Acc)
          end, maps:new(), [NodeMap, NodeInfo, Variables, NodeVariables, ReleaseVariables]),
    maps:fold(
      fun(Key, Value, Acc) ->
              [{atom_to_list(Key), Value}|Acc]
      end, [], VariableMap).

write_file(Dir, Name, Data, CmdOpts) ->
    case Name of 
        "vm.args" ->
            io:format("Data is ~s~n", [Data]);
        _ ->
            ok
    end,
    TempFile = Name ++ ".temp",
    file:write_file(TempFile, Data),
    Filename = filename:join([Dir, Name]),
    io:format("source is ~s file is ~s", [TempFile, Filename]),
    Cmd = cluster_booter_cmd:cmd(write, [{file, Filename}, {source, TempFile}], CmdOpts),
    Result = os:cmd(Cmd),
    io:format("result is ~p~n", [Result]),
    file:delete(TempFile).

    
