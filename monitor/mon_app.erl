-module(mon_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    {ok, F} = file:open("/etc/murdock-id", [read]),
    Cookie = io:get_line(F, ""),
    file:close(F),
    
    erlang:set_cookie(node(), list_to_atom(Cookie)),

    Ret = mon_sup:start_link(),
    gen_event:add_handler(machine_events, machine_logger, []),
    gen_event:add_handler(host_events, host_logger, []),
    gen_event:add_handler(host_events, host_state, []),
    Ret.

stop(_State) -> ok.
