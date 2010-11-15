-module(machines_server).
-behavior(gen_server).
-include_lib("machine_record.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

-record(state, {initialized}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

host2id(Host) ->
    list_to_atom(string:to_lower(Host)).

init(_Args) ->
    ok = util:open_table(machines,
			 [
			  {record_name, machine},
			  {attributes,
			   record_info(fields, machine)},
			  {disc_copies, util:replicas()}
			 ]),

    gen_server:cast(?MODULE, initialize), %defer initialization async to the startup
    {ok, #state{initialized = false}}.

create(Host, Mac) ->
    gen_server:call(?MODULE, {create, Host, Mac}).

lookup(Host) ->
    gen_server:call(?MODULE, {lookup, Host}).

enum() ->
    gen_server:call(?MODULE, enum).

enum_i() ->
    do(qlc:q([X#machine.hostname || X <- mnesia:table(machines)])).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

create_fsm(Host) ->
    Id = host2id(Host),
    StartFunc = {gen_fsm, start_link,
		 [{local, Id}, machine_fsm, [Id, Host], []]},
    supervisor:start_child(hosts_sup,
			   {Id,
			    StartFunc,
			    transient,
			    brutal_kill,
			    worker,
			    [machine_fsm]}),
    {ok, Id}.

handle_call({create, Hostname, Mac}, _From, State) ->
    F = fun() ->
		case mnesia:read(machines, Hostname, write) of
		    [] ->
			Record = #machine{
			  hostname = Hostname,
			  mac = Mac,
			  power = on,
			  state = building
			 },
			mnesia:write(machines, Record, write),
			{created, Record};
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {created, _Record}} ->
	    {ok, Id} = create_fsm(Hostname),
	    {reply, {ok, Id}, State};
	{atomic, exists} ->
	    {reply, exists, State}
    end;
handle_call({lookup, Host}, _From, State) -> 
    F = fun() ->
		mnesia:read(machines, Host, read)
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {reply, notfound, State};
	{atomic, [Record]} ->
	    {reply, {ok, host2id(Host)}, State}
    end;
handle_call(enum, _From, State) ->
    Machines = enum_i(),
    {reply, {ok, Machines}, State};
handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast(initialize, State=#state{initialized = false}) ->
    Hosts = enum_i(),
    [create_fsm(Mac) || Mac <- Hosts],
    {noreply, State#state{initialized = true}};
handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.
