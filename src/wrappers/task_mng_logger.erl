-module(task_mng_logger).

%% API
-export([
  info/2,
  error/2
]).

info(LogStr, Args) ->
    error_logger:info_msg(LogStr, Args).

error(LogStr, Args) ->
    error_logger:error_msg(LogStr, Args).
