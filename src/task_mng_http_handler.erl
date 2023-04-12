-module(task_mng_http_handler).
-behavior(cowboy_http_handler).

%% API
-export([]).

%% API
-export([
    init/3,
    handle/2,
    terminate/3
]).

-define(HEADERS, [
    {<<"Content-Type">>, <<"application/json">>}
]).

-define(INTERNAL_ERR,
    <<"{\"status\":\"error\", \"error_code\":\"internal_error\", \"error_msg\":\"Internal Error\"}">>
).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req1, State) ->
    {Path, Req2} = cowboy_req:path(Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    {ok, Body, Req4} = cowboy_req:body(Req3),
    {InHeaders, Req5} = cowboy_req:headers(Req4),
    {ok, Req6} = try
                     {Code, NewHeaders, Resp} = dispatch(Method, Path, InHeaders, Body),
                     cowboy_req:reply(Code, NewHeaders, Resp, Req5)
                 catch
                     _Type:_Reason ->
%%                         Stacktrace = erlang:get_stacktrace(),
%%                         error_logger:info_msg("Method:~p Path:~p Body:~p~n "
%%                         "Exception Type:~p~n Reason:~p Stacktrace:~p~n",
%%                             [Method, Path, Body, Type, Reason, Stacktrace]),
                         cowboy_req:reply(500, ?HEADERS, ?INTERNAL_ERR, Req4)
                 end,
    {ok, Req6, State}.

terminate(_Reason, _Req, _State) ->
    ok.

dispatch(<<"POST">>, Path, _Headers, Body) ->
%%    error_logger:info_msg("New http query: ~p", [Path]), %% TODO need to add logger - task 16
    case task_mng_coder:decode(Body) of
        {ok, Args} ->
%%            case task_mng_http_req_validator:validate(Path, Args) of %% TODO need to add validation task 14
%%                {ok, Args} ->
                    Result = dispatch(Path, Args),
                    Result2 = encode_result(Result, Path),
                    {ok, Resp} = task_mng_coder:encode(Result2),
                    {200, ?HEADERS, Resp};
%%                {error, Reason} ->
%%%%                    error_logger:info_msg("Args validation failed, path: ~tp, body: ~tp, reason: ~tp ",
%%%%                        [Path, Body, Reason]),
%%                    Message = unicode:characters_to_binary(io_lib:format("Args validation failed: ~tp", [Reason])),
%%                    {400, [{<<"Content-Type">>, <<"text/text">>}], Message}
%%            end;
        {error, Reason} ->
            {415, [{<<"Content-Type">>, <<"text/text">>}], Reason}
    end;
dispatch(<<"GET">>, _Path, _Headers, _Body) ->
    {405, [{<<"Content-Type">>, <<"text/text">>}], <<"POST required">>}.

%% internal
dispatch(Path, _Args) ->
    case Path of
        <<"/task_mng/registration">> ->
            %% some API
            ok;
        <<"/task_mng/login">> ->
            %% some API
            ok;
        <<"/task_mng/logout">> ->
            %% some API
            ok;
        _ ->
            {error, 404, <<"not_valid_path">>}
    end.

encode_result(ok, _Path) ->
    [
        {<<"status">>, <<"ok">>}
    ];
%%encode_result({ok, Result}, _) -> %% TODO uncomment after to do dispatch()
%%    [
%%        {<<"status">>, <<"ok">>},
%%        {<<"response">>, Result}
%%    ];
encode_result({error, Code, Msg}, _) ->
%%    error_logger:info_msg("Path:~p~nError:~p", [Path, Error]),
    [
        {<<"status">>,      <<"error">>},
        {<<"error">>, [
            {<<"code">>, Code},
            {<<"message">>,  Msg}
        ]}
    ].
