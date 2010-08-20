-module(hosts_server).
-behavior(gen_server).
-include_lib("host_record.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([run_once/1,
	 start_link/0, init/1,
	 create/1, lookup/1, get_hostinfo/1, enum/0,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {initialized}).

run_once(Nodes) ->
    io:format("Initializing hosts table~n"),
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
    gen_server:cast(?MODULE, initialize), %defer initialization async to the startup
    {ok, #state{initialized = false}}.

mac2id(Mac) ->
    list_to_atom(string:concat("hostfsm-", Mac)).    

create(Mac) ->
    gen_server:call(?MODULE, {create, Mac}).

lookup(Spec) -> %either {mac, Mac}, or {hostname, Hostname}
    gen_server:call(?MODULE, {lookup, Spec}).

get_hostinfo(Host) ->
    host_fsm:get_hostinfo(Host).

enum() ->
    gen_server:call(?MODULE, enum).

enum_i() ->
    do(qlc:q([X#host.mac || X <- mnesia:table(hosts)])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

create_fsm(Mac) ->
    Id = mac2id(Mac),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, host_fsm, [Id, Mac], []]},
    {ok, _} = supervisor:start_child(hosts_sup,
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
			Record = #host{
			  mac = Mac,
			  hostname = undefined,
			  personality = undefined,
			  power = undefined
			 },
			mnesia:write(hosts, Record, write),
			{created, Record};
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {created, _Record}} ->
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
handle_call(enum, _From, State) ->
    Hosts = enum_i(),
    {reply, {ok, Hosts}, State};
handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast(initialize, State=#state{initialized = false}) ->
    Hosts = enum_i(),
    [create_fsm(Mac) || Mac <- Hosts],
    {noreply, State#state{initialized = true}};
handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

terminate(Reason, State) -> ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
