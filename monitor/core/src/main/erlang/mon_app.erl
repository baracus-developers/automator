-module(mon_app).
-behavior(application).
-export([start/2, stop/1, run_once/0]).

start(_Type, _StartArgs) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
	    ok -> ok;
	    Error ->
		io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
        end;
	_ -> ok
    end,

    Ret = mon_sup:start_link(),
    gen_event:add_handler(machine_events, event_logger, []),
    gen_event:add_handler(host_events, event_logger, []),
    gen_event:add_handler(pool_events, event_logger, []),
    gen_event:add_handler(host_events, host_state, []),
    gen_event:add_handler(machine_events, host_state, []),
    Ret.

stop(_State) -> ok.

run_once() ->
    mnesia:create_schema([node()]).   

