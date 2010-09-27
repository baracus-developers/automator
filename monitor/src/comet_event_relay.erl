-module(comet_event_relay).
-behavior(gen_event).
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_handler/3]).

-record(state, {ref, id, pid, handler}).

add_handler(Ref, Id, Handler) ->
    Init = fun(Pid, State) -> init_job(Pid, State) end,
    Cleanup = fun(State) -> cleanup_job(State) end,
    comet_server:add_job(Init, Cleanup, #state{ref=Ref,
					       id=Id,
					       handler=Handler}).

% init/cleanup runs in the context of the comet_server
init_job(Pid, State) ->
    Ref = State#state.ref,
    Id = State#state.id,
    NewState = State#state{pid=Pid},
    gen_event:add_handler(Ref, {?MODULE, Id}, [NewState]),

    {ok, NewState}.

cleanup_job(State) ->
    Ref = State#state.ref,
    Id = State#state.id,
    gen_event:delete_handler(Ref, {?MODULE, Id}, remove_handler).

% job runs in the context of the comet process    
comet_job(Event, State) ->
    Handler = State#state.handler,
    Handler(Event).

init([State]) ->
    {ok, State}.

handle_event(Event, State) ->
    Pid = State#state.pid,
    Pid ! {execute, fun() -> comet_job(Event, State) end},

    {ok, State}.

handle_call(Call, State) ->
    throw(unexpected).

handle_info(Info, State) ->
    throw(unexpected).

terminate(remove_handler, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
    
