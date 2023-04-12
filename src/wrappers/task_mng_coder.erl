-module(task_mng_coder).

%% API
-export([decode/1, encode/1]).

decode(Body) ->
    Result = jsone:decode(Body),
    {ok, Result}.

encode(Body) ->
    Result = jsone:encode(Body),
    {ok, Result}.
