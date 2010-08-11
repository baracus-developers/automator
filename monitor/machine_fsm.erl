-module(machine_fsm).
-behavior(gen_fsm).
-export([init/1]).
-compile(export_all).

-record(status, {oper, admin}).
-record(state, {id, hostname}).

init([Id, Hostname]) ->
    {ok, building, #state{id = Id, hostname = Hostname}}.

%-------------------------------------------------------

host2id(Host) ->
    list_to_atom(string:to_lower(Host)).

send_event(Host, Event) ->
    Id = host2id(Host),
    gen_fsm:send_event(Id, Event).

reset(Host) ->
    send_event(Host, reset).

poweron(Host) ->
    send_event(Host, poweron).

poweroff(Host) ->
    send_event(Host, poweroff).

terminate(Host) ->
    Id = host2id(Host),
    ok = gen_fsm:sync_send_all_state_event(Id, terminate).

built(Host) ->
    send_event(Host, built).

create(Host) ->
    Id = host2id(Host),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, machine_fsm, [Id, Host], []]},
    supervisor:start_child(mon_sup,
			   {Id,
			    StartFunc,
			    transient,
			    brutal_kill,
			    worker,
			    [machine_fsm]}).

%-------------------------------------------------------
building({agent, offline}, State) ->
    {next_state, building, State};
building(built, State) ->
    {next_state, puppet_join, State, 180000}.

puppet_join(reset, State) ->
    {next_state, building, State};
puppet_join({puppetca, join_request}, State) ->
    puppetca_driver:sign(State#state.hostname),
    {next_state, agent_join, State, 300000};
puppet_join(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for puppet join request"}), 
    {next_state, failed, State}.

failed(reset, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, building, State};
failed(Event, State) ->
    error_logger:warning_msg("~s: Ignoring event ~p in FAILED state~n",
			     [State#state.hostname, Event]),
    {next_state, failed, State}.

agent_join(reset, State) ->
    {next_state, building, State};
agent_join({agent, online, _Since}, State) ->
    {next_state, online, State};
agent_join(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			    "Timeout waiting for ONLINE state"}),
    {next_state, failed, State}.

online(reset, State) ->
    {next_state, building, State};
online({agent, offline}, State) ->
    alarm_handler:set_alarm({State#state.id, "Unexpected OFFLINE state"}),
    {next_state, offline, State}.

offline(reset, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, building, State};
offline({agent, online, _Since}, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, online, State}.

handle_sync_event(terminate, StateName, State) ->
    puppetca_driver:clear(State#state.hostname),
    {stop, terminated, ok, State}.
