-module(agent_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    Monitor = 'monitor@baracus', %FIXME
 
    {ok, F} = file:open("/etc/murdock-id", [read]),
    Cookie = io:get_line(F, ""),
    file:close(F),

    io:format("Cookie: ~p~n", [Cookie]),
    
    erlang:set_cookie(node(), list_to_atom(Cookie)),

    agent_sup:start_link(Monitor).

stop(_State) -> ok.
