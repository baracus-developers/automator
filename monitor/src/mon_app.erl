-module(mon_app).
-behavior(application).
-export([start/2, stop/1, run_once/0]).

start(_Type, _StartArgs) ->

    %{ok, Port} = application:get_env(?MODULE, port),
    Port = 8000,

    Ret = mon_sup:start_link(Port),
    gen_event:add_handler(machine_events, event_logger, []),
    gen_event:add_handler(host_events, event_logger, []),
    gen_event:add_handler(pool_events, event_logger, []),
    gen_event:add_handler(host_events, host_state, []),
    gen_event:add_handler(machine_events, host_state, []),
    Ret.

stop(_State) -> ok.

run_once() ->
    
    Nodes = [node()],

    mnesia:create_schema(Nodes),
    mnesia:start(),

    hosts_server:run_once(Nodes),
    pools_server:run_once(Nodes),
    machines_server:run_once(Nodes),
    baracus_driver:run_once(Nodes),
    puppetca_driver:run_once(Nodes),
    
    init:stop().
