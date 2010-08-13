-module(mon_app).
-behavior(application).
-export([start/2, stop/1, run_once/0]).

start(_Type, _StartArgs) ->
    {ok, F} = file:open("/etc/cloudbuilder-id", [read]),
    Cookie = io:get_line(F, ""),
    file:close(F),
    
    erlang:set_cookie(node(), list_to_atom(Cookie)),

    %{ok, Port} = application:get_env(?MODULE, port),
    Port = 8000,

    Ret = mon_sup:start_link(Port),
    gen_event:add_handler(machine_events, event_logger, []),
    gen_event:add_handler(host_events, event_logger, []),
    gen_event:add_handler(host_events, host_state, []),
    gen_event:add_handler(machine_events, host_state, []),
    gen_event:add_handler(host_events, provisioner, []),
    Ret.

stop(_State) -> ok.

run_once() ->
    
    Nodes = [node()],

    mnesia:create_schema(Nodes),
    mnesia:start(),

    hosts_server:run_once(Nodes),
    baracus_driver:run_once(Nodes),
    puppetca_driver:run_once(Nodes).
