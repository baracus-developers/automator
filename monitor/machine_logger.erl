-module(machine_logger).
-compile(export_all).

init(_Args) ->
    {ok, []}.

handle_event({agent, online, Node, OnlineSince}, State) ->
    io:format("Online: ~p since ~p~n", [Node, OnlineSince]),
    {ok, State};
handle_event({agent, offline, Node}, State) ->
    io:format("Offline: ~p~n", [Node]),
    {ok, State};
handle_event(Event, State) ->
    io:format("Unclassified event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
