-module(host_state).
-behavior(gen_event).
-include_lib("bahost_record.hrl").
-include_lib("certentry.hrl").
-compile(export_all).

init(_Args) ->
    {ok, []}.

handle_agent(Agent, Event) ->
    ["agent", Host] = string:tokens(atom_to_list(Agent), "@"),
    {ok, Id} = machines_server:lookup(Host),
    gen_fsm:send_event(Id, Event),
    ok.

join_request(Host) ->
    {ok, Id} = machines_server:lookup(Host),
    gen_fsm:send_event(Id, {puppetca, join_request}),    
    ok.

handle_event({baracus, add, Mac, Record}, State) ->
    hosts_server:create(Record#bahost.zone, Mac),
    {ok, State};
handle_event({baracus, update, Mac, Old, New}, State) ->
    {ok, Id} = hosts_server:lookup({mac, Mac}),
    gen_fsm:send_event(Id, {baracus_state_change, New#bahost.state}),
    {ok, State};
handle_event({puppetca, add, Host, #certentry{type = request}}, State) ->
    try join_request(Host)
    catch
	_:_ -> error_logger:info_msg("PuppetCA: Ignoring ~s~n", [Host])
    end,
 		       
    {ok, State};
handle_event({agent, online, Agent, Since}, State) ->
    try handle_agent(Agent, {agent, online, Since})
    catch
	_:_ -> error_logger:info_msg("Agent: Ignoring ~p ONLINE event~n",
				     [Agent])
    end,
    {ok, State};
handle_event({agent, offline, Agent}, State) ->
    try handle_agent(Agent, {agent, offline})
    catch
	_:_ -> error_logger:info_msg("Agent: Ignoring ~p OFFLINE event~n",
				     [Agent])
    end,
    {ok, State};
handle_event(Event, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
