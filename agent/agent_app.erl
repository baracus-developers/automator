-module(agent_app).
-behavior(application).
-export([start/2, stop/1]).
-include_lib("xmerl/include/xmerl.hrl").

start(_Type, _StartArgs) ->
 
    {Xml, _} = xmerl_scan:file("/etc/cloudbuilder.conf"),
    [#xmlText{value=Cookie}] = xmerl_xpath:string("//cookie/text()", Xml),
    [#xmlText{value=Contact}] = xmerl_xpath:string("//contacts/contact/text()", Xml),

    erlang:set_cookie(node(), list_to_atom(Cookie)),

    Monitor = list_to_atom(string:concat("monitor@", Contact)),

    io:format("Cookie: ~p Monitor: ~p~n", [Cookie, Monitor]),

    agent_sup:start_link(Monitor).

stop(_State) -> ok.
