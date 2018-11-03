%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_config_base).

%% API
-export([config/2, config_file/3]).

%%%===================================================================
%%% API
%%%===================================================================

config_file(CmdTerms, ConfigFileFlag, DefaultConfigFile) ->
    case proplists:get_value(ConfigFileFlag, CmdTerms) of
        undefined ->
            DefaultConfigFile;
        ConfigFile ->
            ConfigFile
    end.

config(CmdTerms, ConfigFile) ->
    case config_from_file(ConfigFile) of
        {error, Reason} ->
            {error, Reason};
        ConfigTerms ->
            merge_configs(CmdTerms, ConfigTerms)
    end.

config_from_file(ConfigFile) ->
    {ok, CurrentCwd} = file:get_cwd(),
    Config0 = case filelib:is_regular(ConfigFile) of
                  true ->
                      ok = file:set_cwd(filename:dirname(ConfigFile)),
                      Result = case file:consult(ConfigFile) of
                                   {error, Reason} ->
                                       {error, {consult, ConfigFile, Reason}};
                                   {ok, Terms} -> 
                                       {ok, Terms}
                               end,
                      ok = file:set_cwd(CurrentCwd),
                      Result;
                  false -> 
                      {error, no_exists}
              end,
    % we now take the merged config and try to apply a config script to it,
    % get a new config as a result
    case Config0 of
        {error, _} = Error -> Error;
        _ ->
            ConfigScriptFile = config_script_file(ConfigFile),
            case filelib:is_regular(ConfigScriptFile) of
                false -> Config0;
                true -> apply_config_script(Config0, ConfigScriptFile)
            end
    end.

merge_configs([], ConfigTerms) ->
    ConfigTerms;
merge_configs([{_Key, undefined} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{_Key, []} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{Key, Value} | CliTerms], ConfigTerms) ->
    case Key of
        X when X =:= lib_dirs
             ; X =:= goals
             ; X =:= overrides ->
            case lists:keyfind(Key, 1, ConfigTerms) of
                {Key, Value2} ->
                    MergedValue = lists:umerge([Value, Value2]),
                    merge_configs(CliTerms, lists:keyreplace(Key, 1, ConfigTerms, {Key, MergedValue}));
                false ->
                    merge_configs(CliTerms, ConfigTerms++[{Key, Value}])
            end;
        overlay_vars ->
            case lists:keyfind(overlay_vars, 1, ConfigTerms) of
                {_, [H | _] = Vars} when is_list(H) ;
                                         is_tuple(H) ->
                    MergedValue = Vars ++ Value,
                    merge_configs(CliTerms, lists:keyreplace(overlay_vars, 1, ConfigTerms, {Key, MergedValue}));
                {_, Vars} when is_list(Vars) ->
                    MergedValue = [Vars | Value],
                    merge_configs(CliTerms, lists:keyreplace(overlay_vars, 1, ConfigTerms, {Key, MergedValue}));
                false ->
                    merge_configs(CliTerms, ConfigTerms++[{Key, Value}])
            end;
        _ ->
            merge_configs(CliTerms, lists:reverse(lists:keystore(Key, 1, lists:reverse(ConfigTerms), {Key, Value})))
    end.

-spec config_script_file(file:filename()) -> file:filename().
config_script_file(ConfigFile) ->
    ConfigFile ++ ".script".

-spec apply_config_script(proplists:proplist(), file:filename()) ->
                                proplists:proplist().
apply_config_script(ConfigData, ConfigScriptFile) ->
    {ok, Config} = file:script(ConfigScriptFile, bs([{'CONFIG', ConfigData},
                                                     {'SCRIPT', ConfigScriptFile}])),
    Config.

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
