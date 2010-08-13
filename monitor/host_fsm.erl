-module(host_fsm).
-behavior(gen_fsm).
-include_lib("bahost_record.hrl").
-export([init/1, discovery/2]).
-compile(export_all).

-record(status, {oper, admin}).
-record(state, {id, mac, 
		hostname,
		personality
	       }).

init([Id, Mac]) ->
    {ok, discovery,
     #state{id = Id,
	    mac = Mac,
	    personality = #status{oper = none, admin = default}
	   }
    }.

sync_send_event(Mac, Event) ->
    {ok, Id} = host_server:lookup({mac, Mac}),
    gen_fsm:sync_send_event(Id, Event).

configure_power(Mac, Config) ->
    sync_send_event(Mac, {command, {configure_power, Config}}).

provision(Mac, Personality) ->
    sync_send_event(Mac, {command, {provision, Personality}}).

poweron(Mac) ->
    sync_send_event(Mac, {command, poweron}).

poweroff(Mac) ->
    sync_send_event(Mac, {command, poweroff}).

subst(Old, New, Data) ->
    case Data of
	Old ->
	    New;
	_ ->
	    Data
    end.

mac2hostname(Mac) ->
    Hostname = string:concat("cloudbuilder-", 
			     lists:map(fun(X) -> subst($:, $-, X) end, Mac)),
    string:to_lower(Hostname).

handle_provision(Personality, State) ->
    Mac = State#state.mac,
    Hostname = mac2hostname(Mac),

    machine_fsm:create(Hostname),
    baracus_driver:provision(Mac, Hostname, Personality),

    PersonalityStatus = State#state.personality,
    {ok, State#state{personality = PersonalityStatus#status{admin = Personality},
		     hostname = Hostname}}.

handle_reprovision(Personality, State) ->
    PersonalityStatus = State#state.personality,
    if
	Personality =/= PersonalityStatus#status.admin ->
	    machine_fsm:terminate(State#state.hostname),
	    handle_provision(Personality, State);
	true ->
	    nochange
    end.    

illegal_command(Command, StateName, State) ->
    {reply,
     {error, {"Illegal command for state",
	      {state, StateName},
	      {command, Command}
	     }
     },
     StateName, State}.    

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
    baracus_driver:configure_power(State#state.mac, Config),
    baracus_driver:poweroff(State#state.mac),
    {reply, ok, dark, State};
wait({command, Command}, _From, State) ->
    illegal_command(Command, wait, State).

%------------------------------------------------------
% DARK: Node is powered down and can only be awoken
% by a {command, provision} event
%------------------------------------------------------
dark({command, {provision, Personality}}, _From, State) ->
    {ok, NextState} = handle_provision(Personality, State),
    baracus_driver:poweron(State#state.mac),
    {reply, ok, building, NextState};
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
    case handle_reprovision(Personality, State) of
	nochange ->
	    {reply, ok, building, State};
	{ok, NextState} ->
	    baracus_driver:powercycle(State#state.mac),
	    {reply, ok, building, NextState}
    end;
building({command, poweroff}, _From, State) ->
    machine_fsm:terminate(State#state.hostname),
    baracus_driver:poweroff(State#state.mac),
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
    machine_fsm:built(State#state.hostname),

    PersonalityStatus = State#state.personality,
    AdminStatus = PersonalityStatus#status.admin,
    % operstatus is now equal to adminstatus, so update our state
    NewPersonalityStatus = PersonalityStatus#status{oper = AdminStatus},
    {next_state, running, State#state{personality = NewPersonalityStatus}}.

%------------------------------------------------------
% RUNNING: Node is operational
%------------------------------------------------------
running({command, {provision, Personality}}, _From, State) ->
    case handle_reprovision(Personality, State) of
	nochange ->
	    {reply, ok, building, State};
	{ok, NextState} ->
	    baracus_driver:powercycle(State#state.mac),
	    {reply, ok, building, NextState}
    end;
running({command, poweroff}, _From, State) ->
    machine_fsm:poweroff(State#state.hostname),
    baracus_driver:poweroff(State#state.mac),
    {reply, ok, sleep, State};
running({command, Command}, _From, State) ->
    illegal_command(Command, running, State).

%------------------------------------------------------
% SLEEP: Node is powered down and can only be awoken
% by a {command, provision} or {command, poweron} event
%------------------------------------------------------
sleep({command, {provision, Personality}}, _From, State) ->
    case handle_reprovision(Personality, State) of
	nochange ->
	    {reply, ok, building, State};
	{ok, NextState} ->
	    baracus_driver:poweron(State#state.mac),
	    {reply, ok, building, NextState}
    end;
sleep({command, poweron}, _From, State) ->
    machine_fsm:poweron(State#state.hostname),
    baracus_driver:poweron(State#state.mac),
    {reply, ok, running, State};
sleep({command, Command}, _From, State) ->
    illegal_command(Command, sleep, State).

sleep({baracus_state_change, _}, State) ->
    {next_state, sleep, State}.






    

