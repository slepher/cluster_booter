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
-define(DEPS, [installed]).

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
    AllInOne = cluster_booter_state:all_in_one(State),
    BaseDir = cluster_booter_state:root(State),
    CurrentHost = cluster_booter_state:current_host(State),
    Nodes = cluster_booter_state:nodes(State),
    NodeMap = cluster_booter_state:node_map(State),
    Status = cluster_booter_node:check(Nodes, NodeMap),
    NodeVersions = 
        cluster_booter_state:fold_host_nodes(
          fun(Host, Release, Node, Acc) ->
                  CmdOpts = [{host, Host}, {current_host, CurrentHost}],
                  case cluster_booter_state:installed(Host, Node, State) of
                      true ->
                          case cluster_booter_node:started(Node, Status) of
                              true ->
                                  case current_release_version(Node) of
                                      {ok, Version} ->
                                          maps:put(Node, Version, Acc);
                                      {error, _Reason} ->
                                          parse_release_file_acc(AllInOne, BaseDir, Host, Node, Release, CmdOpts, Acc)
                                  end;
                              false ->
                                  parse_release_file_acc(AllInOne, BaseDir, Host, Node, Release, CmdOpts, Acc)
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

current_release_version(TargetNode) ->
    case cluster_booter:rpc_call(TargetNode, release_handler, which_releases, [[], 6000]) of
        {error, Reason} ->
            {error, Reason};
        R ->
            Versions = [ {S, V} ||  {_,V,_, S} <- R ],
            %% current version takes priority over the permanent
            {ok, proplists:get_value(current, Versions,
                                     proplists:get_value(permanent, Versions))}
    end.

format_node_versions(NodeVersions) ->
    maps:fold(
      fun(Node, Version, ok) ->
              io:format("version of ~p is ~p~n", [Node, Version])
      end, ok, NodeVersions).

parse_release_file_acc(AllInOne, BaseDir, Host, Node, Release, CmdOpts, Acc) ->
    case parse_release_file(AllInOne, BaseDir, Node, Release, CmdOpts) of
        {ok, Version} ->
            maps:put(Node, Version, Acc);
        {error, Reason} ->
            io:format("parse version file of ~p at ~s failed ~p~n", [Node, Host, Reason]),
            Acc
    end.

parse_release_file(AllInOne, BaseDir, Node, _Release, CmdOpts) ->
    Filename =
        case AllInOne of
            false ->
                filename:join([BaseDir, Node, "releases/RELEASES"]);
            AllInOne ->
                filename:join([BaseDir, "clients", Node, "releases/RELEASES"])
        end,
    Cmd = cluster_booter_cmd:cmd(read, [{file, Filename}], CmdOpts),
    case os:cmd(Cmd) of
        [] ->
            {error, file_not_exists};
        Content ->
            {ok, Tokens, _EndLine} = erl_scan:string(Content),
            {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
            {value,Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
            case Value of
                [{release, _, Version, _, _, _}] ->
                    {ok, Version};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

