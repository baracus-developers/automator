-module(host_state).
-behavior(gen_event).
-include_lib("bahost_record.hrl").
-include_lib("certentry.hrl").
-compile(export_all).

init(_Args) ->
    {ok, []}.

mac2id(Mac) ->
    %list_to_atom(io_lib:format("host-~s", [Mac])).
    list_to_atom(Mac).

handle_agent(Agent, Event) ->
    ["agent", Host] = string:tokens(atom_to_list(Agent), "@"),
    Id = list_to_existing_atom(string:to_lower(Host)),
    gen_fsm:send_event(Id, Event),
    ok.

join_request(FQDN) ->
    [Host | Domain] = string:tokens(FQDN, "."),
    Id = list_to_existing_atom(string:to_lower(Host)),
    gen_fsm:send_event(Id, {puppetca, join_request}),    
    ok.

handle_event({baracus, add, Mac, Record}, State) ->
    Id = mac2id(Mac),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, host_fsm, [Id, Record], []]},
    supervisor:start_child(mon_sup,
			   {Id,
			    StartFunc,
			    transient,
			    brutal_kill,
			    worker,
			    [host_fsm]}),
    {ok, State};
handle_event({baracus, update, Mac, Old, New}, State) ->
    Id = mac2id(Mac),
    gen_fsm:send_event(Id, {baracus_state_change, New#bahost.state}),
    {ok, State};
handle_event({puppetca, add, FQDN, #certentry{type = request}}, State) ->
    try join_request(FQDN)
    catch
	_:_ -> error_logger:info_msg("PuppetCA: Ignoring ~s~n", [FQDN])
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

provision(Mac, Personality) ->
    gen_fsm:sync_send_event(mac2id(Mac), {command, provision, Personality}).

poweron(Mac) ->
    gen_fsm:sync_send_event(mac2id(Mac), {command, poweron}).

poweroff(Mac) ->
    gen_fsm:sync_send_event(mac2id(Mac), {command, poweroff}).

terminate(_Args, _State) ->
    ok.
