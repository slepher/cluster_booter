%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_prv_versions).


%% API
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, versions).
-define(DEPS, [installed, node_status]).

-spec init(cluster_booter_state:t()) -> {ok, cluster_booter_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {desc, "initialize nodes and start all applications."}
                                ]),
    NState = cluster_booter_state:add_provider(State, Provider),
    {ok, NState}.

do(State) ->
    BaseDir = cluster_booter_state:root(State),
    StartedNodes = cluster_booter_state:started_nodes(State),
    CurrentHost = cluster_booter_state:current_host(State),
    NodeVersions = 
        cluster_booter_state:fold_host_nodes(
          fun(Host, Release, Node, Acc) ->
                  CmdOpts = [{host, Host}, {current_host, CurrentHost}],
                  case cluster_booter_state:installed(Host, Node, State) of
                      true ->
                          case lists:member(Node, StartedNodes) of
                              true ->
                                  Args = [{base_dir, BaseDir}, {node_name, Node}, {release_name, Release}],
                                  Cmd = cluster_booter_cmd:cmd(version, Args, CmdOpts),
                                  VersionResult = os:cmd(Cmd),
                                  case parse_version_result(VersionResult) of
                                      undefined ->
                                          parse_releas_file_acc(BaseDir, Host, Node, Release, Acc);
                                      Version ->
                                          maps:put(Node, Version, Acc)
                                  end;
                              false ->
                                  parse_releas_file_acc(BaseDir, Host, Node, Release, Acc)
                          end;
                      _ ->
                          Acc
                  end
          end, maps:new(), State),
    format_node_versions(NodeVersions),    
    NState = cluster_booter_state:node_versions(State, NodeVersions),
    {ok, NState}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_node_versions(NodeVersions) ->
    maps:fold(
      fun(Node, Version, ok) ->
              io:format("version of ~p is ~p~n", [Node, Version])
      end, ok, NodeVersions).

parse_releas_file_acc(BaseDir, Host, Node, Release, Acc) ->
    case parse_releas_file(BaseDir, Node, Release) of
        {ok, Version} ->
            maps:put(Node, Version, Acc);
        {error, Reason} ->
            io:format("parse version file of ~p at ~s failed ~p~n", [Node, Host, Reason]),
            Acc
    end.

parse_releas_file(BaseDir, Node, Release) ->
    Filename = filename:join([BaseDir, Node, "releases/RELEASES"]), 
    case file:consult(Filename) of
        {ok, [[{release, _, Version, _, _, _}]]} ->
            {ok, Version};
        {error, Reason} ->
            {error, Reason}
    end.
            
parse_version_result(VersionResult) ->
    VersionLines = string:split(VersionResult, "\n", all),
    parse_version_lines(VersionLines).

parse_version_lines([VersionLine|Rest]) ->
    case string:split(VersionLine, " ", all) of
        ["*", Version, "permanent\n"] ->
            Version;
        _ ->
            parse_version_lines(Rest)
    end;
parse_version_lines([]) ->
    undefined.

