-module(ui_sup).
-behavior(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ui_sup}, ui_sup, [Port]).

init(Port) ->
    {ok, {{one_for_one, 1, 60},
	  [
%	   {api_server,
%	    {api_server, start_link, [Port]},
%	    permanent, 5000, worker, [api_server]},
	   {webapp_server,
	    {webapp_server, start_link, [Port]},
	    permanent, 5000, worker, [webapp_server]}
	  ]}}.
