-module(task_mng_db).

-include_lib("epgsql/include/epgsql.hrl").

%% API
-export([
    query/1,
    query/2
]).

-spec query(SQL :: string()) ->
    {ok, {Count :: integer(), Res :: proplists:proplist()}} | %insert ... returning ...
    {ok, []} | %selected data is empty
    {ok, Res :: proplists:proplist()} | %select
    {ok, {Count :: integer(), []}} |  %update
    {error, term()}.  %invalid SQL
query(SQL) ->
    Res = task_mng_db_worker:squery(SQL),
    Log = "[DB] Result of a database query: ~p",
    task_mng_logger:info(Log, Res),
    transform_result(Res).

-spec query(SQL :: string(), Params :: list()) ->
    {ok, {Count :: integer(), Res :: proplists:proplist()}} | %insert ... returning ...
    {ok, []} | %selected data is empty
    {ok, Res :: proplists:proplist()} | %select
    {ok, {Count :: integer(), []}} |  %update
    {error, term()}.  %invalid SQL
query(SQL, Params) ->
    Res = task_mng_db_worker:equery(SQL, Params),
    Log = "[DB] Result of a database query: ~p",
    task_mng_logger:info(Log, Res),
    transform_result(Res).

transform_result(Res) ->
    case Res of
        {ok, Count, Cols, Rows} ->
            {ok, {Count, result_to_proplist(Cols, Rows)}};
        {ok, _Cols, []} ->
            {ok, []};
        {ok, Cols, Rows} ->
            {ok, result_to_proplist(Cols, Rows)};
        {ok, Count} ->
            {ok, {Count, []}};
        {error, Error} ->
            {error, Error}
    end.

result_to_proplist(Cols, Rows) ->
    lists:map(
        fun(Row) ->
            lists:zipwith(
                fun(#column{name = K}, V) -> {K, V} end,
                Cols, tuple_to_list(Row))
        end,
    Rows).
