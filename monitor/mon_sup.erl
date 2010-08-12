-module(mon_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, mon_sup}, mon_sup, [Port]).

init(Port) ->
    {ok, {{one_for_one, 1, 60},
          [{agentmon, {agentmon, start_link, []},
            permanent, brutal_kill, worker, [agentmon]},
	   {host_events,
	    {gen_event, start_link, [{local, host_events}]},
	    permanent, 5000, worker, dynamic},
	   {machine_events,
	    {gen_event, start_link, [{local, machine_events}]},
	    permanent, 5000, worker, dynamic},
	   {bahost_analyzer, {bahost_mon, start_delta_link, []},
            permanent, brutal_kill, worker, [bahost_mon]},
	   {bahost_mon, {bahost_mon, start_poller_link, []},
            permanent, brutal_kill, worker, [bahost_mon]},
	   {puppetca_analyzer, {puppetca_mon, start_delta_link, []},
            permanent, brutal_kill, worker, [puppetca_mon]},
	   {puppetca_mon, {puppetca_mon, start_poller_link, []},
            permanent, brutal_kill, worker, [puppetca_mon]},
	   {api_server,
	    {api_server, start_link, [Port]},
	    permanent, 5000, worker, [api_server]}
	  ]}}.
