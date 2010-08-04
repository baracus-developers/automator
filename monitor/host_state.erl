-module(host_state).
-behavior(gen_event).
-include_lib("bahost_record.hrl").
-compile(export_all).

init(_Args) ->
    {ok, []}.

mac2id(Mac) ->
    %list_to_atom(io_lib:format("host-~s", [Mac])).
    list_to_atom(Mac).

handle_event({baracus, add, Mac, Record}, State) ->
    Id = mac2id(Mac),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, host_fsm, [Id, Record], []]},
    supervisor:start_child(mon_sup,
			   {Id,
			    StartFunc,
			    transient,
			    brutal_kill,
			    worker,
			    [host_fsm]}),
    {ok, State};
handle_event({baracus, update, Mac, Old, New}, State) ->
    Id = mac2id(Mac),
    gen_fsm:send_event(Id, {baracus_state_change, New#bahost.state}),
    {ok, State};
handle_event(Event, State) ->
    error_logger:info_msg("Unexpected event: ~p~n", [Event]),
    {ok, State}.

terminate(_Args, _State) ->
    ok.
