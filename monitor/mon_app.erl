-module(mon_app).
-behavior(application).
-export([start/2, stop/1, run_once/0]).

start(_Type, _StartArgs) ->
    {ok, F} = file:open("/etc/cloudbuilder-id", [read]),
    Cookie = io:get_line(F, ""),
    file:close(F),
    
    erlang:set_cookie(node(), list_to_atom(Cookie)),

    Ret = mon_sup:start_link(),
    gen_event:add_handler(machine_events, machine_logger, []),
    gen_event:add_handler(host_events, host_logger, []),
    gen_event:add_handler(host_events, host_state, []),
    gen_event:add_handler(host_events, provisioner, []),
    Ret.

stop(_State) -> ok.

run_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    bahost_analyzer:run_once(),
    puppetca_analyzer:run_once().
