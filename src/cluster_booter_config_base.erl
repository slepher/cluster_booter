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
-export([default_config/1, merge_configs/2, config/2, config_file/3]).

%%%===================================================================
%%% API
%%%===================================================================

default_config(Opts) ->
    case proplists:get_value(default_config_module, Opts) of
        undefined ->
            [];
        Module ->
            try 
                Module:config()
            catch
                _:_:_ ->
                    []
            end
    end.

config_file(CmdTerms, ConfigFileFlag, DefaultConfigFile) ->
    case proplists:get_value(ConfigFileFlag, CmdTerms) of
        undefined ->
            DefaultConfigFile;
        ConfigFile ->
            ConfigFile
    end.

config(CmdTerms, ConfigFile) ->
    case config_from_file(CmdTerms, ConfigFile) of
        {error, Reason} ->
            {error, Reason};
        {ok, ConfigTerms} ->
            {ok, merge_configs(CmdTerms, ConfigTerms)}
    end.

config_from_file(_CmdTerms, ConfigFile) ->
    Config0 = case filelib:is_regular(ConfigFile) of
                  true ->
                      consult_file(ConfigFile);
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

consult_file(ConfigFile) ->
    case file:consult(ConfigFile) of
        {error, Reason} ->
            {error, {consult, ConfigFile, Reason}};
        {ok, Terms} ->
            case proplists:get_value(variable_file, Terms) of
                undefined ->
                    {ok, Terms};
                VarFile ->
                    case variable_files_vars(VarFile) of
                        {ok, Variables} ->
                            consult_with_variables(Variables, ConfigFile);
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

consult_with_variables(Variables, ConfigFile) ->
    NVariables = lists:map(fun({K, V}) -> {atom_to_list(K), V} end, Variables),
    ConfigTemplate = bbmustache:parse_file(ConfigFile),
    ConfigString = bbmustache:compile(ConfigTemplate, NVariables),
    Configs = cluster_booter_terms:scan_binary(ConfigString),
    {ok, Configs}.


variable_files_vars(VariableFiles) when is_list(VariableFiles) ->
    case lists:nth(1, VariableFiles) of
        VariableFile when is_list(VariableFile) ->
            variable_files_vars_1(VariableFiles, maps:new());
        _ ->
            variable_files_vars_1([VariableFiles], maps:new())
    end.

variable_files_vars_1([ConfigVariableFile|T], Acc) ->
    case variable_file_vars(ConfigVariableFile) of
        {ok, Terms} ->
            TermMap = maps:from_list(Terms),
            TermMap1 = maps:merge(Acc, TermMap),
            variable_files_vars_1(T, TermMap1);
        {error, Reason} ->
            {error, Reason}
    end;
variable_files_vars_1([], Acc) ->
    {ok, maps:to_list(Acc)}.

    
variable_file_vars(ConfigVariableFile) ->
    case filelib:is_regular(ConfigVariableFile) of
        true ->
            case file:consult(ConfigVariableFile) of
                {error, Reason} ->
                    {error, {consult, ConfigVariableFile, Reason}};
                {ok, Terms} ->
                    {ok, Terms}
            end;
        false ->
            {error, {no_variable_file, ConfigVariableFile}}
    end.

merge_configs([], ConfigTerms) ->
    ConfigTerms;
merge_configs([{_Key, undefined} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{_Key, []} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, ConfigTerms);
merge_configs([{Key, Value} | CliTerms], ConfigTerms) ->
    merge_configs(CliTerms, lists:reverse(lists:keystore(Key, 1, lists:reverse(ConfigTerms), {Key, Value}))).

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
