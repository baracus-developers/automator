-module(host_server).
-behavior(gen_server).
-export([run_once/1,
	 start_link/0, init/1,
	 create/1, lookup/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(host, {mac, hostname}).

run_once(Nodes) ->
    {atomic, ok} = mnesia:create_table(hosts,
				       [
					{record_name, host},
					{attributes,
					 record_info(fields, host)},
					{disc_copies, Nodes}
				       ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, unused}.

mac2id(Mac) ->
    list_to_atom(string:concat("hostfsm-", Mac)).    

create(Mac) ->
    gen_server:call(?MODULE, {create, Mac}).

lookup(Spec) -> %either {mac, Mac}, or {hostname, Hostname}
    gen_server:call(?MODULE, {lookup, Spec}).

create_fsm(Mac) ->
    Id = mac2id(Mac),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, host_fsm, [Id, Mac], []]},
    {ok, _} = supervisor:start_child(mon_sup,
				     {Id,
				      StartFunc,
				      transient,
				      brutal_kill,
				      worker,
				      [host_fsm]}),    
    {ok, Id}.

handle_call({create, Mac}, _From, State) ->
    F = fun() ->
		case mnesia:read(hosts, Mac, write) of
		    [] ->
			mnesia:write(hosts, #host{mac = Mac}, write),
			created;
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, created} ->
	    {ok, Id} = create_fsm(Mac),
	    {reply, {ok, Id}, State};
	{atomic, exists} ->
	    {reply, exists, State}
    end;
handle_call({lookup, {mac, Mac}}, _From, State) ->
    F = fun() ->
		mnesia:read(hosts, Mac, read)
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {reply, notfound, State};
	{atomic, [Record]} ->
	    {reply, {ok, mac2id(Mac)}, State}
    end;
handle_call({lookup, {hostname, Hostname}}, _From, State) ->
    erlang:error(notyet);
handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
