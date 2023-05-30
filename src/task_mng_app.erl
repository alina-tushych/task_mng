-module(task_mng_app).

-behaviour(application).

%% API
-export([start/2, stop/1, start_http/0]).

-define(HTTP_PORT, 81).
-define(MaxConnections, 1000).
-define(TcpSendTimeout, 5000).
-define(TcpSendTimeoutClose, 5000).

start(_Type, _StartArgs) ->
%%    application:start(sasl),
%%    application:start(crypto),
%%    application:start(asn1),
%%    application:start(public_key),
%%    application:start(ssl),
%%    application:start(cowlib),
%%    application:start(ranch),
%%    application:start(cowboy),
    Result = task_mng_sup:start_link(),
    start_http(),
%%    task_mng_http_req_validator:init(), %% TODO need to add validation task 19
    Result.

stop(_State) ->
    ok.

start_http() ->
    HttpPort = application:get_env(task_mng, http_port, ?HTTP_PORT), %% TODO maybe to add config file
    Routes = [
        {'_', [
            {"/task_mng/[...]", task_mng_http_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    TransportOpts = #{
        port => HttpPort,
        max_connections => ?MaxConnections,
        send_timeout => ?TcpSendTimeout,
        send_timeout_close => ?TcpSendTimeoutClose    },
    ProtocolOpts = #{
        compress => true,
        env => #{dispatch => Dispatch}
    },
    cowboy:start_clear(task_mng_http, TransportOpts, ProtocolOpts).
