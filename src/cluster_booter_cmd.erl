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
-export([cmd/3]).

%%%===================================================================
%%% API
%%%===================================================================
    
cmd(CmdType, CmdArgs, Opts) ->
    Host = proplists:get_value(host, Opts),
    CurrentHost = proplists:get_value(current_host, Opts),
    IsLocalhost = localhost(Host, CurrentHost),
    host_cmd(Host, IsLocalhost, CmdType, CmdArgs).

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
    
host_cmd(Host, IsLocalHost, CmdType, Args) ->
    Cmd = cmd(CmdType, Args),
    case IsLocalHost of
        true ->
            tunnel_cmd(Cmd);
        false ->
            ssh_cmd(Host, Cmd)
    end.

tunnel_cmd({tunnel, Before, Cmd}) ->
    Before ++ " | " ++ Cmd;
tunnel_cmd(Cmd) when is_list(Cmd) ->
    Cmd.

ssh_cmd(Host, {tunnel, Before, Cmd}) ->
    tunnel_cmd({tunnel, Before, ssh_cmd(Host, Cmd)});
ssh_cmd(Host, Cmd) when is_list(Cmd) ->
    lists:flatten("ssh " ++ Host ++ " \"" ++ string:replace(Cmd, "\"", "\\\"", all) ++ "\" 2>/dev/null").

cmd(processes, _) ->
    "ps aux | grep setcookie | sed 's/  */ /g' | cut -f 11- -d ' '";
cmd(start, Args) ->
    NodeName = proplists:get_value(node_name, Args),
    ReleaseName = proplists:get_value(release_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    Start = filename:join([BaseDir, NodeName, "bin", ReleaseName]),
    Start ++ " start";
cmd(start_boot, Args) ->
    NodeName = proplists:get_value(node_name, Args),
    ReleaseName = proplists:get_value(release_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    BootFile = proplists:get_value(boot_file, Args),
    Start = filename:join([BaseDir, NodeName, "bin", ReleaseName]),
    Start ++ " start_boot " ++ BootFile;
cmd(extract, Args) ->
    Release = proplists:get_value(release, Args),
    Version = proplists:get_value(version, Args),
    PackagesPath = proplists:get_value(packages_path, Args),
    Filename = filename:join(PackagesPath, atom_to_list(Release) ++ "-" ++ Version ++ ".tar.gz"),
    Nodename = proplists:get_value(node_name, Args),
    BaseDir = proplists:get_value(base_dir, Args),
    {tunnel, "cat " ++ Filename, " tar zxf - -C "  ++ filename:join(BaseDir, Nodename)};
cmd(mkdir, Args) ->
    Dir = proplists:get_value(dir, Args),
    "mkdir -p " ++ Dir;
cmd(exists, Args) ->
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