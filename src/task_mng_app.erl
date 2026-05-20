-module(task_mng_app).

-behaviour(application).

%% API
-export([start/2, stop/1, start_http/0]).

-define(HTTP_PORT, 8080).

start(_Type, _StartArgs) ->
    Result = task_mng_sup:start_link(),
    start_http(),
%%    task_mng_http_req_validator:init(), %% TODO need to add validation task 19
    Result.

stop(_State) ->
    ok.

start_http() ->
    HttpPort = application:get_env(task_mng, http_port, ?HTTP_PORT),
    Routes = [
        {'_', [
            {"/task_mng/[...]", task_mng_http_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    TransportOpts = [{port, HttpPort}],
    ProtocolOpts = #{
        compress => true,
        env => #{dispatch => Dispatch}
    },
    cowboy:start_clear(task_mng_http, TransportOpts, ProtocolOpts).
