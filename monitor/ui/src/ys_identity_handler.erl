-module (ys_identity_handler).
-behaviour (identity_handler).
-export ([
    init/2, 
    finish/2,
    get_user/2, 
    set_user/3,
    clear/2
]).

init(_Config, State) -> 
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.

get_user(Ctx, _State) -> 
    yaws_security_context:principal(Ctx).

set_user(_User, _Ctx, _State) -> 
    throw(unsupported).

clear(Ctx, State) -> 
    ok = yaws_security_context:clear(Ctx),
    {ok, State}.
