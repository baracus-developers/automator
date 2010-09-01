-module(host_fsm).
-behavior(gen_fsm).
-include_lib("host_record.hrl").
-include_lib("hostinfo.hrl").
-export([init/1, discovery/2]).
-compile(export_all).

-record(state, {id, mac}).

init([Id, Mac]) ->
    {ok, initialize, #state{id = Id, mac = Mac}, 0}.

%------------------------------------------------------------------------

sync_send_event(Mac, Event) ->
    {ok, Id} = hosts_server:lookup({mac, Mac}),
    gen_fsm:sync_send_event(Id, Event).

configure_power(Mac, Config) ->
    sync_send_event(Mac, {command, {configure_power, Config}}).

provision(Mac, Personality) ->
    sync_send_event(Mac, {command, {provision, Personality}}).

poweron(Mac) ->
    sync_send_event(Mac, {command, poweron}).

poweroff(Mac) ->
    sync_send_event(Mac, {command, poweroff}).

get_hostinfo(Mac) ->
    {ok, Id} = hosts_server:lookup({mac, Mac}),
    Id ! {get_hostinfo, self()},
    receive
	Msg -> Msg
    after 10000 ->
	    timeout
    end.

%------------------------------------------------------------------------

handle_info({get_hostinfo, From}, running, State) ->
    {ok, MachineInfo} = machine_fsm:info(get_hostname(State)),
    From ! {ok, #hostinfo{mac = State#state.mac, state = MachineInfo}},
    {next_state, running, State};
handle_info({get_hostinfo, From}, StateName, State) ->
    From ! {ok, #hostinfo{mac = State#state.mac, state = StateName}},
    {next_state, StateName, State}.

subst(Old, New, Data) ->
    case Data of
	Old ->
	    New;
	_ ->
	    Data
    end.

compute_hostname(Mac) ->
    {ok, Domain} = config:get_domain(),
    Hostname = "cloudbuilder-" ++ 
	lists:map(fun(X) -> subst($:, $-, X) end, Mac) ++
	"." ++ Domain,
    string:to_lower(Hostname).

get_record(State) when erlang:is_record(State, state) ->
    Mac = State#state.mac,
    get_record(Mac);
get_record(Mac) ->
    F = fun() ->
		mnesia:read(hosts, Mac, read)
	end,
    case mnesia:transaction(F) of
	{atomic, [Record]} when is_record(Record, host) ->
	    {ok, Record}
    end.

get_hostname(State) when erlang:is_record(State, state) ->
    Mac = State#state.mac,
    get_hostname(Mac);
get_hostname(Mac) ->
    {ok, Record} = get_record(Mac),
    Record#host.hostname.

handle_provision(Personality, State) ->
    Mac = State#state.mac,
    FQDN = compute_hostname(Mac),
    [Hostname | Domain] = string:tokens(FQDN, "."),

    machines_server:create(FQDN, Mac),
    baracus_driver:provision(Mac, Hostname, Personality),

    F = fun() ->
		[Record] = mnesia:read(hosts, Mac, write),
		mnesia:write(hosts,
			     Record#host{personality = Personality,
					 hostname = FQDN,
					 power = on},
			     write),
		updated
	end,
    {atomic, updated} = mnesia:transaction(F),
    ok.

handle_reprovision(Personality, State) ->
    Hostname = get_hostname(State),
    machine_fsm:delete(Hostname),
    handle_provision(Personality, State).

illegal_command(Command, StateName, State) ->
    {reply,
     {error, {"Illegal command for state",
	      {state, StateName},
	      {command, Command}
	     }
     },
     StateName, State}.    

handle_poweroff(Mac) ->
    baracus_driver:poweroff(Mac),

    F = fun() ->
		[Record] = mnesia:read(hosts, Mac, write),
		mnesia:write(hosts, Record#host{power = off}, write),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

initialize(timeout, State) ->
    Mac = State#state.mac,
    {ok, BaracusState} = baracus_driver:get_state(Mac),
    {ok, Record} = get_record(Mac),

    error_logger:info_msg("~p: (Re)starting with Record: ~p BaracusState: ~p~n",
			  [State#state.id, Record, BaracusState]),

    initialize(Record, BaracusState, State).

initialize(Record=#host{hostname = undefined, personality = undefined, power = undefined},
	   inventory, State) ->
    {next_state, discovery, State};
initialize(Record=#host{power = on}, BaracusState, State)
  when BaracusState =:= built; BaracusState =:= localboot ->

    Mac = Record#host.mac,

    case baracus_driver:powerstatus(Mac) of
	on -> void;
	_ -> baracus_driver:poweron(Mac)
    end,

    {next_state, running, State}.


%------------------------------------------------------
% DISCOVERY: We enter this state while we wait for
% baracus to gather the hardware inventory
%------------------------------------------------------
discovery({baracus_state_change, register}, State) ->
    Mac = State#state.mac,
    Inventory = baracus_driver:get_inventory(Mac),
    gen_event:notify(host_events, {system, discovery, {Mac, Inventory}}),
    {next_state, wait, State}.

%------------------------------------------------------
% WAIT: We enter this state while we wait for
% a higher layer to configure our power subsystem
%------------------------------------------------------
wait({command, {configure_power, Config}}, _From, State) ->
    Mac = State#state.mac,
    baracus_driver:configure_power(Mac, Config),
    handle_poweroff(Mac),
    {reply, ok, dark, State};
wait({command, Command}, _From, State) ->
    illegal_command(Command, wait, State).

%------------------------------------------------------
% DARK: Node is powered down and can only be awoken
% by a {command, provision} event
%------------------------------------------------------
dark({command, {provision, Personality}}, _From, State) ->
    Mac = State#state.mac,
    ok = handle_provision(Personality, State),
    baracus_driver:poweron(Mac),
    {reply, ok, building, State};
dark({command, Command}, _From, State) ->
    illegal_command(Command, dark, State).

dark({baracus_state_change, _}, State) ->
    {next_state, dark, State}.

%------------------------------------------------------
% BUILDING: Node is powered on and in the process of
% being imaged.  We will transition to RUNNING once
% we receive a BUILT state from Baracus.  We will
% transition to SLEEP if a {command, poweroff} arrives.
% Finally, we will powercycle the node if a
% {command, provision} arrives but remain in BUILDING
%------------------------------------------------------
building({command, {provision, Personality}}, _From, State) ->
    Mac = State#state.mac,
    handle_reprovision(Personality, Mac),
    baracus_driver:powercycle(Mac),
    {reply, ok, building, State};
building({command, poweroff}, _From, State) ->
    Mac = State#state.mac,
    Hostname = get_hostname(Mac),

    machine_fsm:delete(Hostname),
    handle_poweroff(Mac),

    {reply, ok, dark, State};
building({command, Command}, _From, State) ->
    illegal_command(Command, building, State).

building({baracus_state_change, build}, State) ->
    {next_state, building, State};
building({baracus_state_change, building}, State) ->
    {next_state, building, State}; 
building({baracus_state_change, localboot}, State) ->
    {next_state, building, State};
building({baracus_state_change, built}, State) ->
    Hostname = get_hostname(State),

    machine_fsm:built(Hostname),

    {next_state, running, State}.

%------------------------------------------------------
% RUNNING: Node is operational
%------------------------------------------------------
running({command, {provision, Personality}}, _From, State) ->
    Mac = State#state.mac,

    handle_reprovision(Personality, Mac),
    baracus_driver:powercycle(Mac),

    {reply, ok, building, State};
running({command, poweroff}, _From, State) ->
    Mac = State#state.mac,
    Hostname = get_hostname(Mac),

    machine_fsm:poweroff(Hostname),
    handle_poweroff(Mac),

    {reply, ok, sleep, State};
running({command, Command}, _From, State) ->
    illegal_command(Command, running, State).

running({baracus_state_change, localboot}, State) ->
    {next_state, running, State}.


%------------------------------------------------------
% SLEEP: Node is powered down and can only be awoken
% by a {command, provision} or {command, poweron} event
%------------------------------------------------------
sleep({command, {provision, Personality}}, _From, State) ->
    Mac = State#state.mac,

    handle_reprovision(Personality, Mac),
    baracus_driver:poweron(Mac),

    {reply, ok, building, State};
sleep({command, poweron}, _From, State) ->
    Mac = State#state.mac,
    Hostname = get_hostname(Mac),

    machine_fsm:poweron(Hostname),
    baracus_driver:poweron(Mac),

    F = fun() ->
		[Record] = mnesia:read(hosts, Mac, write),
		mnesia:write(hosts, Record#host{power = on}, write),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),

    {reply, ok, running, State};
sleep({command, Command}, _From, State) ->
    illegal_command(Command, sleep, State).

sleep({baracus_state_change, _}, State) ->
    {next_state, sleep, State}.






    

