-module(staging_sup).
-behavior(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{one_for_all, 0, 1},
          [
	   {staging_server, {staging_server, start_link, []},
            permanent, brutal_kill, worker, [staging_server]}
	  ]}}.
