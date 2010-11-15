-module(event_logger).
-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.

% special case some events so we dont get that huge XML on the log/console
handle_event({Principal, stagingnode, updated, {Mac, _, _}}, State) ->
    error_logger:info_msg("Event: ~p node updated by ~p~n", [Mac, Principal]),
    {ok, State};
handle_event(Event, State) ->
    error_logger:info_msg("Event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
