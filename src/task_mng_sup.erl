-module(task_mng_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(STOP_CHILD_TIMEOUT, 5000).
-define(CHILD(I, Type, Args),
    #{
        id => I,
        start => {
            I,
            start_link,
            Args
        },
        restart => permanent,
        shutdown => ?STOP_CHILD_TIMEOUT,
        type => Type
    }
).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
        strategy    => one_for_one,
        intensity   => 500,
        period      => 10
    },
    MainWorker = ?CHILD(task_mng_worker, worker, []),
    DbConfig   = application:get_env(task_mng, database, []),
    DBWorker   = ?CHILD(task_mng_db_worker, worker, [DbConfig]),
    Workers = [MainWorker, DBWorker],
    {ok, {SupFlags, Workers}}.
