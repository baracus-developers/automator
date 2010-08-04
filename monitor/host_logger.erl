-module(host_logger).
-include_lib("bahost_record.hrl").
-compile(export_all).

init(_Args) ->
    {ok, []}.

handle_event({baracus, update, Mac, Old, New}, State) ->
    io:format("Update: ~p from ~p to ~p~n",
	      [Mac, Old#bahost.state, New#bahost.state]),
    {ok, State};
handle_event({baracus, add, Mac, Record}, State) ->
    io:format("Add: ~p as ~p~n", [Mac, Record#bahost.state]),
    {ok, State};
handle_event({policy, provision, Mac, Hostname}, State) ->
    io:format("Provision: ~s as ~s~n", [Mac, Hostname]),
    {ok, State};
handle_event(Event, State) ->
    io:format("Unclassified event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
