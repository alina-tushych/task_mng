-module(task_mng_coder).

%% API
-export([decode/1, decode/2, encode/1]).

decode(Body) ->
    Result = jsone:decode(Body),
    {ok, Result}.

decode(Body, Options) ->
    Result = jsone:decode(Body, Options),
    {ok, Result}.

encode(Body) ->
    Result = jsone:encode(Body),
    {ok, Result}.
