-module(event_logger).
-export([init/1, handle_event/2, terminate/2]).

init(_Args) ->
    {ok, []}.

handle_event({system, discovery, {Mac, _Inventory}}, State) ->
    % special case the discovery event so we dont get that huge XML on the log/console
    error_logger:info_msg("Event: {system, discovery, ~p}~n", [Mac]),
    {ok, State};
handle_event(Event, State) ->
    error_logger:info_msg("Event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
