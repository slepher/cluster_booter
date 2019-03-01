%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_cmd).

%% API
-export([cmd/2, cmd/3]).

%%%===================================================================
%%% API
%%%===================================================================
cmd(Cmd, Opts) ->
    Host = proplists:get_value(host, Opts),
    CurrentHost = proplists:get_value(current_host, Opts),
    host_cmd(Host, CurrentHost, Cmd).

cmd(CmdType, CmdArgs, Opts) ->
    Cmd = gen_cmd(CmdType, CmdArgs),
    cmd(Cmd, Opts).

localhost(undefined, _) ->
    true;
localhost("127.0.0.1", _) ->
    true;
localhost("localhost", _) ->
    true;
localhost(Host, Host) ->
    true;
localhost(_Host, _CurrentHost) ->
    false.
    
host_cmd(Host, CurrentHost, Cmd) ->
    IsLocalhost = localhost(Host, CurrentHost),
    case IsLocalhost of
        true ->
            local_cmd(Cmd);
        false ->
            remote_cmd(Host, Cmd)
    end.

local_cmd({rsync, From, To, Options}) ->
    lists:flatten(io_lib:format("rsync -av ~s ~s ~s", [Options, From, To]));
local_cmd({tunnel, Before, Cmd}) ->
    Before ++ " | " ++ Cmd;
local_cmd(Cmd) when is_list(Cmd) ->
    Cmd.

remote_cmd(Host, {rsync, From, To, Options}) ->
    lists:flatten(io_lib:format("rsync -av ~s ~s ~s:~s", [Options, From, Host, To]));
remote_cmd(Host, {tunnel, Before, Cmd}) ->
    local_cmd({tunnel, Before, remote_cmd(Host, Cmd)});
remote_cmd(Host, Cmd) when is_list(Cmd) ->
    lists:flatten("ssh " ++ Host ++ " \"" ++ string:replace(Cmd, "\"", "\\\"", all) ++ "\" 2>/dev/null").

gen_cmd(processes, _) ->
    "ps aux | grep setcookie | sed 's/  */ /g' | cut -f 11- -d ' '";
gen_cmd(start, Args) ->
    NodeName = proplists:get_value(node_name, Args),
    ReleaseName = proplists:get_value(release_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    Start = filename:join([BaseDir, NodeName, "bin", ReleaseName]),
    Start ++ " start";
gen_cmd(start_boot, Args) ->
    NodeName = proplists:get_value(node_name, Args),
    ReleaseName = proplists:get_value(release_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    Start = filename:join([BaseDir, NodeName, "bin", ReleaseName]),
    Start ++ " start_boot load";
gen_cmd(extract, Args) ->
    Filename = proplists:get_value(filename, Args),
    TargetDirectory = proplists:get_value(target_directory, Args),
    ExtractType = proplists:get_value(extract_type, Args, "zxf"),
    Strip = integer_to_list(proplists:get_value(strip, Args, 0)),
    {tunnel, "cat " ++ Filename, " tar " ++ ExtractType ++ " - --strip-components=" ++ Strip ++ " -C "  ++ TargetDirectory};
gen_cmd(mkdir, Args) ->
    Dir = proplists:get_value(dir, Args),
    "[ -d " ++ Dir ++ " ] || mkdir -p " ++ Dir;
gen_cmd(version, Args) ->
    NodeName = proplists:get_value(node_name, Args),
    ReleaseName = proplists:get_value(release_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    File = filename:join([BaseDir, NodeName, "bin", ReleaseName]),
    File ++ " versions";
gen_cmd(write, Args) ->
    Source = proplists:get_value(source, Args),
    File = proplists:get_value(file, Args),
    {tunnel, "cat " ++ Source, "cat - >" ++ File};
gen_cmd(read, Args) ->
    File = proplists:get_value(file, Args),
    "[ -f " ++ File ++ " ] && cat " ++ File;
gen_cmd(exists, Args) ->
    BaseDir = proplists:get_value(base_dir, Args),
    Dirname = 
        case proplists:get_value(node_name, Args) of
            undefined ->
                BaseDir;
            Nodename ->
                filename:join(BaseDir, Nodename)
        end,
    "[ -d " ++ Dirname ++ " ] && echo ok".
    
    


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
