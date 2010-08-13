-module(agentmon).
-behavior(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_info/2]).

-record(state, {nodes}).
-record(node, {ref, hostname, online_since, pid}).

start_link() ->
    gen_server:start_link({global, agentmon}, agentmon, [], []).

init([]) ->
    {ok, #state{nodes = dict:new()}}.

add_node(Pid, Since, State) ->
    Ref = erlang:monitor(process, Pid),
    Hostname = erlang:node(Pid),
    Node = #node{ref          = Ref,
		 hostname     = Hostname,
		 online_since = Since,
		 pid          = Pid
		},
    Nodes = dict:store(Ref, Node, State#state.nodes),
    gen_event:notify(machine_events, {agent, online, Hostname, Since}),
    State#state{nodes = Nodes}.

drop_node(Node, State) ->
    Ref = Node#node.ref,
    Nodes = dict:erase(Ref, State#state.nodes),
    gen_event:notify(machine_events, {agent, offline, Node#node.hostname}),
    State#state{nodes = Nodes}.

handle_call({nodeup, Since}, {From, _}, State) -> 
    {reply, ok, add_node(From, Since, State)};
handle_call(Request, _, State) ->
    {reply, {badarg, Request}, State}.
		  
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case dict:find(Ref, State#state.nodes) of
	{ok, Node} ->
	    {noreply, drop_node(Node, State)};
	error ->
	    erlang:error({"Unknown 'DOWN' request", Ref})
    end.


			       
    
