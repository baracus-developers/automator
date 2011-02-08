-module(hosts_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_all, 1, 60},
          [
	   {hosts_server, {hosts_server, start_link, []},
            permanent, brutal_kill, worker, [hosts_server]},
	   {pools_server, {pools_server, start_link, []},
            permanent, brutal_kill, worker, [pools_server]},
	   {machines_server, {machines_server, start_link, []},
            permanent, brutal_kill, worker, [machines_server]}
	  ]}}.
