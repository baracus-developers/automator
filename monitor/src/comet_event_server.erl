-module(comet_event_server).
-behavior(gen_server).

-export([
    start_link/0, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-record(state, {pid}).

start_link() ->
    CallerPid = self(),
    {ok, Pid} = gen_server:start(?MODULE, [CallerPid], []),
    
    link(Pid),

    {ok, Pid}.

init([Pid]) ->
    process_flag(trap_exit, true),
    S = self(),
    gen_event:add_handler(host_events, {comet_event_relay, S}, [Pid]),
    gen_event:add_handler(machine_events, {comet_event_relay, S}, [Pid]),
    {ok, #state{pid=Pid}}.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(Message, State) ->
    {noreply, State}.

handle_info({'EXIT', From, async_die}, State) when From =:= State#state.pid ->
    S = self(),
    gen_event:delete_handler(host_events, {comet_event_relay, S}, remove_handler),
    gen_event:delete_handler(machine_events, {comet_event_relay, S}, remove_handler),
    {stop, normal, State}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
