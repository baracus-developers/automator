-module(agent_connection_fsm).
-behavior(gen_fsm).
-export([init/1, start_link/1, handle_info/3, terminate/3]).
-export([firstconnect/2, reconnecting/2]).

-record(state, {monitor, online_since}).

start_link(Monitor) ->
    gen_fsm:start_link({local, agent_connection}, agent_connection_fsm, Monitor, []).

init([Monitor]) ->
    InitialState = #state{monitor = Monitor, online_since = erlang:now()},
    {ok, firstconnect, InitialState, 0}.

connect(State) ->
    connect(State, reconnecting, 5000).

connect(State, FailState, Timeout) ->
    net_adm:ping(State#state.monitor),
    case global:whereis_name(agentmon) of 
    	undefined ->
	    {next_state, FailState, State, Timeout};
	Pid ->
	    error_logger:info_msg("Connected~n"),
	    erlang:monitor(process, Pid),
	    ok = gen_server:call(Pid, {nodeup, State#state.online_since}),
	    {next_state, connected, State, hibernate}  
    end.

firstconnect(timeout, State) ->
    connect(State, firstconnect, 500).

reconnecting(timeout, State) ->
    connect(State).

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, connected, State) ->
    error_logger:info_msg("Disconnected~n"),
    connect(State).

terminate(_Reason, _State, _Data) ->
    error_logger:info_msg("Terminating~n"),
    void.
    
	    

 		  
