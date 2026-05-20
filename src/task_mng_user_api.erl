-module(task_mng_user_api).

%% API
-export([
    register/1,
    login/1,
    logout/1,
    delete/1
]).

register(Args) ->
    Login = maps:get(<<"login">>, Args),
    Password = maps:get(<<"password">>, Args),
    Role = maps:get(<<"role">>, Args),
    SQL = "INSERT INTO public.users (login, password, role, work_status, deleted) "
        "VALUES ($1, $2, $3, $4, $5) "
        "ON CONFLICT(login) "
        "DO NOTHING "
        "RETURNING user_id;",
    case task_mng_db:query(SQL, [Login, Password, Role, <<"noactive">>, <<"no">>]) of
        {ok, {1, _Res}} ->
            ok;
        {ok, {0, []}} ->
            {error, 409, <<"user_already_exists">>};
        _ ->
            {error, 500, <<"unknown_error">>}
    end.

login(Args) ->
    Login = maps:get(<<"login">>, Args),
    Password = maps:get(<<"password">>, Args),
    SQL1 = "SELECT user_id FROM public.users WHERE login = $1 and password = $2 and deleted = $3;",
    case task_mng_db:query(SQL1, [Login, Password, <<"no">>]) of
        {ok, []} ->
            {error, 404, <<"user_not_found_or_deleted">>};
        {ok, [Res]} ->
            UserId = proplists:get_value(<<"user_id">>, Res),
            SQL2 = "UPDATE public.users SET last_login = now() WHERE user_id = $1;",
            case task_mng_db:query(SQL2, [UserId]) of
                {ok, _Res2} ->
                    add_user_activities(UserId);
                _Error ->
                    {error, 500, <<"unknown_error">>}
            end;
        _ ->
            {error, 500, <<"unknown_error">>}
    end.

logout(Args) ->
    UserId = maps:get(<<"user_id">>, Args),
    SQL1 = "SELECT login_dt FROM public.activities WHERE user_id = $1;",
    case task_mng_db:query(SQL1, [UserId]) of
        {ok, []} ->
            {error, 401, <<"user_not_logged_in">>};
        {ok, _Res} ->
            make_logout(UserId);
        _ ->
            {error, 500, <<"unknown_error">>}
    end.

delete(Args) ->
    UserId = maps:get(<<"user_id">>, Args),
    SQL1 = "SELECT * FROM public.users WHERE user_id = $1;",
    case task_mng_db:query(SQL1, [UserId]) of
        {ok, []} ->
            {error, 404, <<"user_not_found">>};
        {ok, _Res} ->
            Role = maps:get(<<"role">>, Args),
            case Role of
                <<"admin">> ->
                    delete_user_by_admin(UserId);
                _ ->
                    {error, 403, <<"access_deny">>}
            end;
        _ ->
            {error, 500, <<"unknown_error">>}
    end.

%% =====================================================================================================================
%% internal
%% =====================================================================================================================

add_user_activities(UserId) ->
    SQL3 = "INSERT INTO public.activities (user_id, login_dt) VALUES ($1, now());",
    case task_mng_db:query(SQL3, [UserId]) of
        {ok, _Res} ->
            ok;
        _Error ->
            {error, 404, <<"user_not_found">>}
    end.

make_logout(UserId) ->
    SQL2 = "UPDATE public.activities SET logout_dt = now() WHERE user_id = $1;",
    case task_mng_db:query(SQL2, [UserId]) of
        {ok, _Res} ->
            ok;
        _ ->
            {error, 404, <<"user_not_found">>}
    end.

delete_user_by_admin(UserId) ->
    SQL2 = "UPDATE public.users SET deleted = 'yes' WHERE user_id = $1;",
    case task_mng_db:query(SQL2, [UserId]) of
        {ok, _Res} ->
            case make_logout(UserId) of
                ok ->
                    ok;
                _ ->
                    {error, 409, <<"user_already_logout">>}
            end;
        _ ->
            {error, 500, <<"unknown_error">>}
    end.
