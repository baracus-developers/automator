-module(host_fsm).
-behavior(gen_fsm).
-include_lib("bahost_record.hrl").
-export([init/1, discovery/2]).
-compile(export_all).

-record(status, {oper, admin}).
-record(state, {id, mac, 
		hostname,
		baracus_state,
		power_state,
		personality
	       }).

initial_state(State) ->
    case State of
	inventory ->
	    discovery;
	built ->
	    running
    end.

init([Id, Record]) ->
    {ok,
     initial_state(Record#bahost.state),
     #state{id = Id,
	    mac = Record#bahost.mac,
	    baracus_state = Record#bahost.state,
	    power_state = #status{oper = true, admin = true}
	   }
    }.

subst(Old, New, Data) ->
    case Data of
	Old ->
	    New;
	_ ->
	    Data
    end.

discovery({baracus_state_change, register}, State) ->
    Mac = State#state.mac,
    Hostname = lists:map(fun(X) -> subst($:, $-, X) end, Mac),
    baracus_driver:provision(Mac, Hostname, State#state.personality),
    {next_state, building, State}.

building({baracus_state_change, built}, State) ->
    {next_state, joining, State, 60000};
building(_, State) ->
    {next_state, building, State}. 

joining(puppet_join_request, State) ->
    {next_state, running, State, 60000};
joining(timeout, State) ->
    error_logger:warning_msg("~p: Timeout waiting for puppet join~n",
			     [State#state.id]),
    {next_state, running, State}.

running({agent_state_change, online}, State) ->
    {next_state, online, State};
running(timeout, State) ->
    error_logger:warning_msg("~p: Timeout waiting for agent join~n",
			     [State#state.id]),
    {next_state, running, State};
running({baracus_state_change, _}, State) ->
    {next_state, running, State}.

online({agent_state_change, offline}, State) ->
    error_logger:error_msg("~p: Unexpected offline transition~n",
			   [State#state.id]),
    {next_state, offline, State}.

offline({agent_state_change, online}, State) ->
    error_logger:info_msg("~p: Node is online~n", [State#state.id]),
    {next_state, online, State}.


    

