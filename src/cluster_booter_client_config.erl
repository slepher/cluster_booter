%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 12 Jun 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_client_config).

%% API
-export([generate/5]).

%%%===================================================================
%%% API
%%%===================================================================
generate(Host, Release, Version, Node, State) ->
    AllInOne = cluster_booter_state:all_in_one(State),
    BaseDir = cluster_booter_state:root(State),
    CurrentHost = cluster_booter_state:current_host(State),
    SysConfig = cluster_booter_state:sys_config(State),
    VMArgs = cluster_booter_state:vm_args(State),
    EnvFile = cluster_booter_state:erl_env(State),
    CmdOpts = [{host, Host}, {current_host, CurrentHost}],

    SysConfigTemplate = bbmustache:parse_file(SysConfig),
    VMArgsTemplate = bbmustache:parse_file(VMArgs),
    EnvTemplate = bbmustache:parse_file(EnvFile),
    FVariables = update_node_variables(Release, Node, State),
    SysConfigResult = bbmustache:compile(SysConfigTemplate, FVariables),
    VMArgResult = bbmustache:compile(VMArgsTemplate, FVariables),
    EnvResult = bbmustache:compile(EnvTemplate, FVariables),
    BaseDir1 = 
        case AllInOne of
            false ->
                BaseDir;
            AllInOne ->
                filename:join([BaseDir, "clients"])
        end,
    Dir = filename:join([BaseDir1, Node, "releases", Version]),
    write_file(Dir, "sys.config", SysConfigResult, CmdOpts),
    write_file(Dir, "vm.args", VMArgResult, CmdOpts),
    write_file(filename:join([BaseDir1, Node, "bin"]), "erl.env", EnvResult, CmdOpts),
    chmod(filename:join([BaseDir1, Node, "bin", "erl.env"]), CmdOpts).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
update_node_variables(Release, NodeName, State) ->
    Root = cluster_booter_state:root(State),
    MnesiaDir = cluster_booter_state:mnesia_dir(State),
    LogDir = cluster_booter_state:log_dir(State),
    PipeDir = cluster_booter_state:pipe_dir(State),
    Cookie = cluster_booter_state:cookie(State),
    NodeMap = cluster_booter_state:node_map(State),
    Variables = cluster_booter_state:variables(State),
    NodeVariablesMap = cluster_booter_state:node_variables(State),
    IsNode = list_to_atom(lists:flatten(io_lib:format("is_~p", [NodeName]))),
    IsRelease = list_to_atom(lists:flatten(io_lib:format("is_~p", [Release]))),
    Node = maps:get(NodeName, NodeMap),
    NodeInfo = #{IsNode => true, IsRelease => true, node_name => NodeName,
                 node => Node, release_name => Release, cookie => Cookie, root => Root,
                 mnesia_dir => MnesiaDir, log_dir => LogDir, pipe_dir => PipeDir},
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
    TempFile = Name ++ ".temp",
    file:write_file(TempFile, Data),
    Filename = filename:join([Dir, Name]),
    Cmd = cluster_booter_cmd:cmd(write, [{file, Filename}, {source, TempFile}], CmdOpts),
    os:cmd(Cmd),
    file:delete(TempFile).

chmod(File, CmdOpts) ->
    Cmd = cluster_booter_cmd:cmd("chmod +x " ++ File, CmdOpts),
    os:cmd(Cmd).
    
