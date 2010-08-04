-module(mon_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, mon_sup}, mon_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 1, 60},
          [{agentmon, {agentmon, start_link, []},
            permanent, brutal_kill, worker, [agentmon]},
	   {host_events,
	    {gen_event, start_link, [{local, host_events}]},
	    permanent, 5000, worker, dynamic},
	   {machine_events,
	    {gen_event, start_link, [{local, machine_events}]},
	    permanent, 5000, worker, dynamic},
	   {bahost_analyzer, {bahost_analyzer, start_link, []},
            permanent, brutal_kill, worker, [bahost_analyzer]},
	   {bahost_mon, {bahost_mon, start_link, []},
            permanent, brutal_kill, worker, [bahost_mon]}
	  ]}}.
