-module(task_mng_http_handler).

%% API
-export([]).

%% API
-export([
    init/2,
    terminate/3
]).

-define(HEADERS, #{
    <<"Content-Type">> => <<"application/json">>
}).

-define(INTERNAL_ERR,
    <<"{\"status\":\"error\",\"error\":{\"code\":500,\"message\":\"Internal Server Error\"}}">>
).

init(Req1, State) ->
    Path = cowboy_req:path(Req1),
    Method = cowboy_req:method(Req1),
    {ok, Body, Req2} = cowboy_req:read_body(Req1),
    InHeaders = cowboy_req:headers(Req2),
    Log = "[HTTP] Incomming request:~nMethod:~p~nPath:~p~nBody:~p~n",
    Arg = [Method, Path, Body],
    task_mng_logger:info(Log, Arg),
    {Code, Headers, Resp} =
        try
            {_Code, _NewHeaders, _Resp} = dispatch(Method, Path, InHeaders, Body)
        catch
            error:_Reason ->
                {500, ?HEADERS, ?INTERNAL_ERR};
            throw:_Reason ->
                {500, ?HEADERS, ?INTERNAL_ERR};
            exit:_Reason ->
                {500, ?HEADERS, ?INTERNAL_ERR}
        end,
    RespLog = "[HTTP] Request result:~nMethod:~p~nPath:~p~nBody:~p~n"
        "Code:~p~nHeaders:~p Resp:~p~n",
    RespArg = [Method, Path, Body, Code, Headers, Resp],
    task_mng_logger:info(RespLog, RespArg),
    Req3 = cowboy_req:reply(Code, Headers, Resp, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

dispatch(<<"POST">> = Method, Path, _Headers, Body) ->
    DecodeBody = task_mng_coder:decode(Body, [{object_format, map}]),
    Log = "[HTTP] Incoming request~nMethod:~p~nPath:~p~nBody:~p~n",
    Arg = [Method, Path, DecodeBody],
    task_mng_logger:info(Log, Arg),
    case DecodeBody of
        {ok, Args} ->
%%            case task_mng_http_req_validator:validate(Path, Args) of %% TODO need to add validation task 19
%%                {ok, Args} ->
                    Result = dispatch(Path, Args),
                    OkLog = "[HTTP] Response OK~nMethod:~p~nPath:~p~nBody:~p~nResult:~p",
                    OkArg = [Method, Path, DecodeBody, Result],
                    task_mng_logger:info(OkLog, OkArg),
                    Result2 = encode_result(Result, Path),
                    {ok, Resp} = task_mng_coder:encode(Result2),
                    {200, ?HEADERS, Resp};
%%                {error, Reason} ->
%%                    ErrLog = "[HTTP] Args validation failed~nMethod:~p~nPath:~p~nBody:~p~nReason:~p",
%%                    task_mng_logger:error(ErrLog, [Method, Path, Body, Reason]),
%%                    Message = unicode:characters_to_binary(io_lib:format("Args validation failed: ~p", [Reason])),
%%                    {400, [{<<"Content-Type">>, <<"text/text">>}], Message}
%%            end;
        {error, Reason} ->
            ErrLog = "[HTTP] Request ERROR~nMethod:~p~nPath:~p~nBody:~p~n Reason:~p",
            ErrArg = [Method, Path, DecodeBody, Reason],
            task_mng_logger:error(ErrLog, ErrArg),
            {415, #{<<"Content-Type">> => <<"text/text">>}, <<"Bed body">>}
    end;
dispatch(<<"GET">> = Method, Path, _Headers, Body) ->
    ErrLog = "[HTTP] Request ERROR~nMethod:~p~nPath:~p~nBody:~p~n Reason:POST required",
    ErrArg = [Method, Path, Body],
    task_mng_logger:error(ErrLog, ErrArg),
    {405, #{<<"Content-Type">> => <<"text/text">>}, <<"POST required">>}.

%% =====================================================================================================================
%% internal
%% =====================================================================================================================

dispatch(Path, Args) ->
    case Path of
        <<"/task_mng/registration">> ->
            task_mng_user_api:register(Args);
        <<"/task_mng/login">> ->
            task_mng_user_api:login(Args);
        <<"/task_mng/logout">> ->
            task_mng_user_api:logout(Args);
        <<"/task_mng/delete">> ->
            task_mng_user_api:delete(Args);
        _ ->
            {error, 404, <<"not_valid_path">>}
    end.

encode_result(ok, _Path) ->
    [
        {<<"status">>, <<"ok">>}
    ];
encode_result({error, Code, Msg}, _) ->
    [
        {<<"status">>,      <<"error">>},
        {<<"error">>, [
            {<<"code">>, Code},
            {<<"message">>,  Msg}
        ]}
    ].
