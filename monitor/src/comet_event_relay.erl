-module(comet_event_relay).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pid}).

init([Pid]) ->
    {ok, #state{pid=Pid}}.

handle_event(Event, State) ->
    Pid = State#state.pid,
    Pid ! {relay, Event},

    {ok, State}.

handle_call(Call, State) ->
    throw(unexpected).

handle_info(Info, State) ->
    throw(unexpected).

terminate(remove_handler, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
    
