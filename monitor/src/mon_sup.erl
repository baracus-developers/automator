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
	   {pool_events,
	    {gen_event, start_link, [{local, pool_events}]},
	    permanent, 5000, worker, dynamic},
	   {machine_events,
	    {gen_event, start_link, [{local, machine_events}]},
	    permanent, 5000, worker, dynamic},
	   {config, {config, start_link, []},
            permanent, brutal_kill, worker, [config]},
	   {bahost_analyzer, {baracus_driver, start_link, []},
            permanent, brutal_kill, worker, [baracus_driver]},
	   {bahost_mon, {bahost_mon, start_link, []},
            permanent, brutal_kill, worker, [bahost_mon]},
	   {staging_sup, {staging_sup, start_link, []},
            permanent, brutal_kill, supervisor, [staging_sup]},
	   {puppetca_analyzer, {puppetca_driver, start_link, []},
            permanent, brutal_kill, worker, [puppetca_driver]},
	   {puppetca_mon, {puppetca_mon, start_link, []},
            permanent, brutal_kill, worker, [puppetca_mon]},
	   {hosts_sup, {hosts_sup, start_link, []},
            permanent, brutal_kill, supervisor, [hosts_sup]},
%	   {api_server,
%	    {api_server, start_link, [Port]},
%	    permanent, 5000, worker, [api_server]},
	   {webapp_server,
	    {webapp_server, start_link, [Port]},
	    permanent, 5000, worker, [webapp_server]}
	  ]}}.
