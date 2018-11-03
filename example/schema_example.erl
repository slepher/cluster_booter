%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(schema_example).

%% API
-compile({parse_transform, mnesia_schema_transform}).

%% API
-export([applications/0, tables/0]).

-record(record_a, {id, record_a_field_a}).
-record(record_b, {id, record_b_field_a}).
-record(record_c, {id, record_c_field_a}).
-record(record_d, {id, record_d_field_a}).
-record(record_e, {id, record_e_field_a}).
-record(record_f, {id, record_f_field_a}).
-record(record_g, {id, record_g_field_a}).

%%%===================================================================
%%% API
%%%===================================================================

applications() ->
    #{
      application_a => [table_group_a, table_group_c],
      application_b => [table_group_b, table_group_c]
     }.

tables() ->
    #{
      table_group_a => 
          mnesia_schema(
            record_a,
            #record_b{indexes = [record_b_filed_a], table = table_b},
            #record_c{init_data = record_cs(), type = bag}
           ),
      table_group_b =>
          mnesia_schema(
            record_d,
            record_e,
            record_f
           ),
      table_group_c =>
          mnesia_schema(
            record_g
           )
     }.

record_cs() ->
    [#record_c{id = 1}, #record_c{id = 2}, #record_c{id = 3}].
      

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
