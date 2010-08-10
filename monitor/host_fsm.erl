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

initial_state(State) ->
    case State of
	inventory ->
	    discovery;
	built ->
	    running;
	_ ->
	    erlang:error({"Code is unprepared to handle initial state", State})
    end.

init([Id, Record]) ->
    {ok,
     initial_state(Record#bahost.state),
     #state{id = Id,
	    mac = Record#bahost.mac,
	    personality = #status{oper = none, admin = default}
	   }
    }.

subst(Old, New, Data) ->
    case Data of
	Old ->
	    New;
	_ ->
	    Data
    end.

sleepyness() -> 60000.

handle_provision(Personality, State) ->
    Mac = State#state.mac,
    Hostname = string:concat("cloudbuilder-", 
			     lists:map(fun(X) -> subst($:, $-, X) end, Mac)),

    machine_fsm:create(Hostname),
    baracus_driver:provision(Mac, Hostname, Personality),

    Domainname = ".laurelwood.net", %FIXME
    puppetca_driver:clear(string:concat(Hostname, Domainname)),

    PersonalityStatus = State#state.personality,
    {reply, ok, building,
     State#state{personality = PersonalityStatus#status{admin = Personality},
		 hostname = Hostname}}.

handle_reprovision(Personality, State) ->
    PersonalityStatus = State#state.personality,
    if
	Personality =/= PersonalityStatus#status.admin ->
	    Reply = handle_provision(Personality, State),
	    baracus_driver:powercycle(State#state.mac),
	    Reply;
	true ->
	    {reply, ok, building, State}
    end.    

handle_poweron(State) ->
    PersonalityStatus = State#state.personality,
    if
	PersonalityStatus#status.oper =/= PersonalityStatus#status.admin ->
	    handle_provision(PersonalityStatus#status.admin, State);
	true ->
	    {reply, ok, running, State}
    end.

handle_poweroff(State) ->
    machine_fsm:poweroff(State#state.hostname),
    baracus_driver:poweroff(State#state.mac),
    {reply, ok, deepsleep, State}.

%------------------------------------------------------
% DISCOVERY: We enter this state while we wait for
% baracus to gather the hardware inventory
%------------------------------------------------------
discovery({baracus_state_change, register}, State) ->
    Mac = State#state.mac,
    Inventory = baracus_driver:get_inventory(Mac),
    gen_event:notify(host_events, {system, discovery, {Mac, Inventory}}),
    error_logger:info_msg("Discovered ~p, powering down in ~pms~n",
			  [Mac, sleepyness()]),
    {next_state, sleepy, State, sleepyness()}.

%------------------------------------------------------
% SLEEPY: We enter this state for N seconds after discovery
% to give automation systems a chance to invoke {command, provision}
% before we actually power down the node.  Once we time-out, we
% enter DEEPSLEEP
%------------------------------------------------------
sleepy({command, provision, Personality}, _From, State) ->
    handle_provision(Personality, State);
sleepy({command, poweron}, _From, State) ->
    handle_poweron(State);
sleepy({command, poweroff}, _From, State) ->
    baracus_driver:poweroff(State#state.mac),
    {reply, ok, deepsleep, State}.

sleepy(timeout, State) ->
    baracus_driver:poweroff(State#state.mac),
    {next_state, deepsleep, State}.

%------------------------------------------------------
% DEEPSLEEP: Node is powered down and can only be awoken
% by a {command, provision} or {command, poweron} event
%------------------------------------------------------
deepsleep({command, provision, Personality}, _From, State) ->
    NextState = handle_provision(Personality, State),
    baracus_driver:poweron(State#state.mac),
    NextState;
deepsleep({command, poweron}, _From, State) ->
    NextState = handle_poweron(State),
    baracus_driver:poweron(State#state.mac),
    NextState;
deepsleep({command, poweroff}, _From, State) ->
    {reply, {error, "Already powered down"}, deepsleep, State}.

deepsleep({baracus_state_change, _}, State) ->
    {next_state, deepsleep, State}.

%------------------------------------------------------
% BUILDING: Node is powered on and in the process of
% being imaged.  We will transition to RUNNING once
% we receive a BUILT state from Baracus.  We will
% transition to DEEPSLEEP if a {command, poweroff} arrives.
% Finally, we will powercycle the node if a
% {command, provision} arrives but remain in BUILDING
%------------------------------------------------------
building({command, provision, Personality}, _From, State) ->
    handle_reprovision(Personality, State);
building({command, poweroff}, _From, State) ->
    handle_poweroff(State);
building({command, poweron}, _From, State) ->
    {reply, {error, "Already running"}, building, State}.

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

running({command, provision, Personality}, _From, State) ->
    handle_reprovision(Personality, State);
running({command, poweroff}, _From, State) ->
    handle_poweroff(State);
running({command, poweron}, _From, State) ->
    {reply, {error, "Already running"}, building, State}.





    

