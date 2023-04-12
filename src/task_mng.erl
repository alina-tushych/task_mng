-module(task_mng).

%% API
-export([
    start/0,
    stop/0
]).

start() ->
    {ok, _} = application:ensure_all_started(task_mng, permanent).

stop() ->
    application:stop(task_mng).
