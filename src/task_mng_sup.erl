-module(task_mng_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(STOP_CHILD_TIMEOUT, 2000).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, ?STOP_CHILD_TIMEOUT, Type, []}).
-define(DB_ARGS, [
    {hostname, "localhost"},
    {database, "task_mng"},
    {username, "postgres"},
    {password, "postgres"},
    {port, 5432}
]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
        strategy    => one_for_one,
        intensity   => 500,
        period      => 10
    },
    MainWorker = ?CHILD(task_mng_worker, worker, []),
    DBWorker   = ?CHILD(task_mng_db_worker, worker, ?DB_ARGS),
    Workers = [MainWorker, DBWorker],
    {ok, {SupFlags, Workers}}.
