-module(task_mng_db_worker).

-behaviour(gen_server).

-export([
    start_link/1,
    squery/1,
    equery/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

squery(SQL) ->
    gen_server:call(?MODULE, {squery, SQL}).

equery(SQL, Params) ->
    gen_server:call(?MODULE, {equery, SQL, Params}).

init(Args) ->
    process_flag(trap_exit, true),
    Hostname = proplists:get_value(hostname, Args, "localhost"),
    Database = proplists:get_value(database, Args, "task_mng"),
    Username = proplists:get_value(username, Args, "postgres"),
    Password = proplists:get_value(password, Args, "postgres"),
    Port     = proplists:get_value(port, Args, 5432),
    Options = [{database, Database}, {port, Port}],
    {ok, Conn} = epgsql:connect(Hostname, Username, Password, Options),
    {ok, #state{conn = Conn}}.

handle_call({squery, Sql}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:squery(Conn, Sql), State};
handle_call({equery, Stmt, Params}, _From, #state{conn = Conn} = State) ->
    {reply, epgsql:equery(Conn, Stmt, Params), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    ok = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
