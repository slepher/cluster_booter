%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2021, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Jan 2021 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_file_lib).

%% API
-export([consult_clusup/1]).
-export([copy_clusfile/1]).

%%%===================================================================
%%% API
%%%===================================================================
consult_clusup(ClusupFile) ->
    case file:consult(ClusupFile) of
        {ok,[{clusup, ClusterName, ToVsn, FromVsn, Changes}]} ->
            {ok, {clusup, ClusterName, ToVsn, FromVsn, Changes, []}};
        {ok, [{clusup, ClusterName, ToVsn, FromVsn, Changes, Extra}]} ->
            {ok,{clusup, ClusterName, ToVsn, FromVsn, Changes, Extra}};
        {error, Reason} ->
            {error, {consult_file_failed, ClusupFile, Reason}}
    end.

copy_clusfile(State) ->
    Cwd = file:get_cwd(),
    AllInOne = cluster_booter_state:all_in_one(State),
    ClusName = AllInOne ++ ".clus",
    UpVsn = cluster_booter_state:version(State),
    From = filename:join([Cwd, "releases", UpVsn, ClusName]),
    To = filename:join([Cwd, "releases", ClusName]),
    file:copy(From, To).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
