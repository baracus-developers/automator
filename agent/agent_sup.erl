-module(agent_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Monitor) ->
    supervisor:start_link(agent_sup, [Monitor]).

init(Monitor) ->
    {ok, {{one_for_one, 0, 1},
	  [{agent_connection_fsm,
	    {agent_connection_fsm, start_link, [Monitor]},
            permanent, brutal_kill, worker, [agent_connection_fsm]}]}}.
