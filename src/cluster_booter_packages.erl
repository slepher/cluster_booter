%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  8 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cluster_booter_packages).

%% API
-export([set_installed/4, installed/3, format_installed/1]).

%%%===================================================================
%%% API
%%%===================================================================
set_installed(Host, Release, Installed, InstalledPackages) ->
    HostInstalled = maps:get(Host, Installed, maps:new()),
    NHostInstalled = maps:put(Release, Installed, HostInstalled),
    maps:put(Host, NHostInstalled, InstalledPackages).

installed(Host, Release, InstalledPackages) ->
   case maps:find(Host, InstalledPackages) of
       {ok, HostInstalled} ->
           case maps:find(Release, HostInstalled) of
               {ok, Installed} ->
                   Installed;
               error ->
                   unknown
           end;
       error ->
           unknown
   end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
format_installed(InstalledPackages) ->
    maps:fold(
      fun(Host, NodeMap, ok) ->
              maps:fold(
                fun(Node, Status, ok) ->
                        StatusStr = 
                            case Status of
                                true ->
                                    "installed";
                                false ->
                                    "not_installed"
                            end,
                        io:format("~p is ~s at host ~s~n", [Node, StatusStr, Host])
                end, ok, NodeMap)
      end, ok, InstalledPackages).
