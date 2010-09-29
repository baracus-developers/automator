-module(genevent_bridge).
-behavior(gen_event).

-export([init/1, handle_event/2]).

-export([add_handler/3, delete_handler/2]).

-record(state, {serverref}).

add_handler(EventRef, Id, ServerRef) ->
    gen_event:add_handler(EventRef, {?MODULE, Id}, ServerRef).

delete_handler(EventRef, Id) ->
    gen_event:delete_handler(EventRef, {?MODULE, Id}).

init(ServerRef) ->
    {ok, #state{serverref=ServerRef}}.

handle_event(Event, State) ->
    gen_server:cast(State#state.serverref, {genevent_bridge, Event}),
    {ok, State}.

handle_call(Request, State) ->
    throw(not_implemented).

handle_info(Info, State) ->
    throw(not_implemented).

terminate(Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
