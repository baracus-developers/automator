-module(machine_fsm).
-behavior(gen_fsm).

-include("machine_record.hrl").
-include("certentry.hrl").
-include("dhcp.hrl").

-export([init/1, built/1, poweron/1, poweroff/1, reboot/1, delete/1]).
-compile(export_all).

-record(status, {oper, admin}).
-record(state, {id, mac, hostname}).

init([Id, Mac, FQDN]) ->
    [Hostname | _] = string:tokens(FQDN, "."),
    Domain = string:substr(FQDN, length(Hostname) + 2),
    DHCPOptions = [
		   {?DHO_HOST_NAME, Hostname},
		   {?DHO_DOMAIN_NAME, Domain}
		  ],
    ok = dhcp_server:add_client(Mac, DHCPOptions),

    {ok, initialize, #state{id=Id, mac=Mac, hostname=FQDN}, 0}.

%-------------------------------------------------------

send_event(Host, Event) ->
    {ok, Id} = machines_server:lookup(Host),
    gen_fsm:send_event(Id, Event).

built(Host) ->
    send_event(Host, built).

poweron(Host) ->
    send_event(Host, poweron).

poweroff(Host) ->
    send_event(Host, poweroff).

reboot(Host) ->
    send_event(Host, reboot).

delete(Host) ->
    {ok, Id} = machines_server:lookup(Host),
    ok = gen_fsm:sync_send_all_state_event(Id, delete).

info(Host) ->
    {ok, Id} = machines_server:lookup(Host),
    Id ! {get_info, self()},
    receive
	Msg -> Msg
    after 10000 ->
	    timeout
    end.

%-------------------------------------------------------

handle_info({get_info, From}, StateName, State) ->
    From ! {ok, StateName},
    {next_state, StateName, State}.

puppet_sign(State) ->
    Host = State#state.hostname,
    puppetca_driver:sign(Host),
    update_state(Host, ready).

get_record(State) when erlang:is_record(State, state) ->
    Host = State#state.hostname,
    get_record(Host);
get_record(Host) ->
    F = fun() ->
		mnesia:read(machines, Host, read)
	end,
    case mnesia:transaction(F) of
	{atomic, [Record]} when is_record(Record, machine) ->
	    {ok, Record}
    end.

update_state(Host, State) ->
    F = fun() ->
		[Record] = mnesia:read(machines, Host, write),
		mnesia:write(machines, Record#machine{state=State},
			     write),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

update_power(Host, State) ->
    F = fun() ->
		[Record] = mnesia:read(machines, Host, write),
		mnesia:write(machines, Record#machine{power=State},
			     write),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

std_timeout() -> 120000.
puppet_timeout() -> 180000.
sync_timeout() -> 300000.
initial_startup_timeout() -> 300000.

%-------------------------------------------------------

initialize(timeout, State) ->
    {ok, Record} = get_record(State),

    PersistState = Record#machine.state,
    PowerState = Record#machine.power,
    {ok, BaracusState} = baracus_driver:get_state(Record#machine.mac),
    PuppetState = puppetca_driver:find(State#state.hostname),
    AgentState = agentmon:find(State#state.hostname),

    initialize(PersistState, PowerState, BaracusState,
	       PuppetState, AgentState, State).

initialize(building, off, _, _, _, State) ->
    {next_state, building_down, State};

initialize(building, on, BaracusState, notfound, offline, State) when 
  BaracusState =/= built ->
    {next_state, building, State, 1200000};

initialize(joining, off, built, notfound, offline, State) ->
    {next_state, puppet_down, State};

initialize(joining, on, built, notfound, offline, State) ->
    {next_state, puppet_join, State, puppet_timeout()};

initialize(joining, on, built,
	   {ok, CertEntry=#certentry{type = request}}, offline, State) ->
    puppetca_driver:sign(State#state.hostname),
    {next_state, synchronizing, State, initial_startup_timeout()};

initialize(joining, on, built, {ok, CertEntry=#certentry{type = valid}},
	   offline, State) ->
    {next_state, synchronizing, State, initial_startup_timeout()};

initialize(ready, on, BaracusState, {ok, CertEntry=#certentry{type = valid}},
	   {online, Since}, State)
  when BaracusState =:= built; BaracusState =:= localboot ->
    {next_state, online, State};

initialize(ready, on, BaracusState, {ok, CertEntry=#certentry{type = valid}},
	   offline, State)
  when BaracusState =:= built; BaracusState =:= localboot ->
    {next_state, synchronizing, State, initial_startup_timeout()};

initialize(ready, off, BaracusState, {ok, CertEntry=#certentry{type = valid}},
	   _, State) 
  when BaracusState =:= built; BaracusState =:= localboot ->
    {next_state, down, State}.

% ...and hopefully, all other combos are invalid states

%-------------------------------------------------------

building(poweroff, State) ->
    update_power(State#state.hostname, off),
    {next_state, building_down, State};
building(reboot, State) ->
    {next_state, building, State};
building(built, State) ->
    update_state(State#state.hostname, joining),
    {next_state, puppet_join, State, puppet_timeout()};
building({puppetca, join_request}, State) ->
    {next_state, building_joined, State, 10000};
building(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for build completion"}), 
    {next_state, building_failed, State}.

building_failed(poweroff, State) ->
    alarm_handler:clear_alarm(State#state.id),
    update_power(State#state.hostname, off),
    {next_state, building_down, State};
building_failed(reboot, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, building, State};
building_failed(built, State) ->
    alarm_handler:clear_alarm(State#state.id),
    update_state(State#state.hostname, joining),
    {next_state, puppet_join, State, puppet_timeout()}.

building_down(built, State) ->
    {next_state, puppet_down, State};
building_down(poweron, State) ->
    update_power(State#state.hostname, on),
    {next_state, building, State}.

building_joined(built, State) ->
    puppet_sign(State),
    {next_state, synchronizing, State, sync_timeout()};
building_joined(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for build completion"}), 
    {next_state, building_joined_failed, State}.

building_joined_failed(built, State) ->
    puppet_sign(State),
    {next_state, synchronizing, State, sync_timeout()}.

puppet_join({puppetca, join_request}, State) ->
    puppet_sign(State),
    {next_state, synchronizing, State, sync_timeout()};
puppet_join(poweroff, State) ->
    update_power(State#state.hostname, off),
    {next_state, puppet_down, State};
puppet_join(reboot, State) ->
    {next_state, puppet_join, State, puppet_timeout()};
puppet_join(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for puppet join request"}), 
    {next_state, puppet_failed, State}.

puppet_failed({puppetca, join_request}, State) ->
    alarm_handler:clear_alarm(State#state.id),
    puppet_sign(State),
    {next_state, synchronizing, State, sync_timeout()};
puppet_failed(reboot, State) ->
    {next_state, puppet_failed, State};
puppet_failed(powerdown, State) ->
    update_power(State#state.hostname, off),
    {next_state, puppet_failed_down, State}.
    
puppet_down(poweron, State) ->
    update_power(State#state.hostname, on),
    {next_state, puppet_join, State, puppet_timeout()};
puppet_down({puppetca, join_request}, State) ->
    puppet_sign(State),
    {next_state, down, State}.

puppet_failed_down(poweron, State) ->
    update_power(State#state.hostname, on),
    {next_state, puppet_failed, State};
puppet_failed_down({puppetca, join_request}, State) ->
    alarm_handler:clear_alarm(State#state.id),
    puppet_sign(State),
    {next_state, down, State}.

synchronizing(reboot, State) ->
    {next_state, synchronizing, State, std_timeout()};
synchronizing({agent, online, _Since}, State) ->
    {next_state, online, State};
synchronizing(poweroff, State) ->
    update_power(State#state.hostname, off),
    {next_state, down, State};
synchronizing(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for startup"}),
    {next_state, offline, State}.

online({agent, offline}, State) ->
    alarm_handler:set_alarm({State#state.id, "Unexpected OFFLINE state"}),
    {next_state, offline, State};
online(reboot, State) ->
    {next_state, reboot, State, std_timeout()};
online(poweroff, State) ->
    update_power(State#state.hostname, off),
    {next_state, shutting_down, State, std_timeout()}.

offline({agent, online, _Since}, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, online, State};
offline(reboot, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, synchronizing, State, std_timeout()};
offline(poweroff, State) ->
    alarm_handler:clear_alarm(State#state.id),
    update_power(State#state.hostname, off),
    {next_state, down, State}.

shutting_down(poweron, State) ->
    update_power(State#state.hostname, on),
    {next_state, reboot, State, std_timeout()};
shutting_down({agent, offline}, State) ->
    {next_state, down, State};
shutting_down(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for shutdown"}),
    {next_state, shutting_down_failed, State}.

shutting_down_failed(poweron, State) ->
    alarm_handler:clear_alarm(State#state.id),
    update_power(State#state.hostname, on),
    {next_state, reboot, State, std_timeout()};
shutting_down_failed({agent, offline}, State) -> 
    alarm_handler:clear_alarm(State#state.id),
    {next_state, down, State}.

reboot(poweroff, State) ->
    update_power(State#state.hostname, off),
    {next_state, shutting_down, State, std_timeout()};
reboot({agent, offline}, State) ->
    {next_state, synchronizing, State, std_timeout()};
reboot(reboot, State) ->
    {next_state, reboot, State, std_timeout()};
reboot(timeout, State) ->
    alarm_handler:set_alarm({State#state.id,
			     "Timeout waiting for reboot"}),
    {next_state, reboot_failed, State}.

reboot_failed(poweroff, State) ->
    alarm_handler:clear_alarm(State#state.id),
    update_power(State#state.hostname, off),
    {next_state, shutting_down, State, std_timeout()};
reboot_failed(reboot, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, reboot, State, std_timeout()};
reboot_failed({agent, offline}, State) ->
    alarm_handler:clear_alarm(State#state.id),
    {next_state, synchronizing, State, std_timeout()}.

down(poweron, State) ->
    update_power(State#state.hostname, on),
    {next_state, synchronizing, State, std_timeout()}.

handle_sync_event(delete, StateName, State) ->
    puppetca_driver:clear(State#state.hostname),
    F = fun() ->
		mnesia:delete(machines, State#state.hostname, write),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    {stop, terminated, ok, State}.

terminate(Reason, State) ->
    ok = dhcp_server:remove_client(State#state.mac),
    alarm_handler:clear_alarm(State#state.id).
    
