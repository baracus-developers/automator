-module(pools_server).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-export([run_once/1,
	 start_link/0, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/1, enum/0]).

-record(state, {}).

-record(member, {mac, pool, category}).
-record(pool, {name, system, created_on, rules}).



run_once(Nodes) ->
    io:format("Initializing pools tables~n"),
    {atomic, ok} = mnesia:create_table(members,
				       [
					{record_name, member},
					{attributes,
					 record_info(fields, member)},
					{disc_copies, Nodes}
				       ]),
    {atomic, ok} = mnesia:create_table(pools,
				       [
					{record_name, pool},
					{attributes,
					 record_info(fields, pool)},
					{disc_copies, Nodes}
				       ]),
    create_pool("Default", true).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    genevent_bridge:add_handler(host_events, self(), self()),
    {ok, #state{}}.

create(Pool) ->
    gen_server:call(?MODULE, {create, Pool}).

enum() ->
    gen_server:call(?MODULE, enum).

enum_i() ->
    do(qlc:q([X#pool.name || X <- mnesia:table(pools)])).

assign_to_pool(Mac, Pool, Category, State) -> 
    F = fun() ->
		[#pool{name=Pool}] = mnesia:read(pools, Pool, read),

		Record = case mnesia:read(members, Mac, write) of
			     [] ->
				 #member{mac = Mac,
					 pool = Pool, 
					 category = Category};
			     [OrigRecord] ->
				 OrigRecord#member{pool = Pool,
						   category = Category}
			 end,
		mnesia:write(members, Record, write),
		ok
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(pool_events,
			     {system, pool_assignment, Mac, Pool, Category}),
	    ok;
	Else ->
	    {error, Else}
    end.

create_pool(Pool, System) ->
    F = fun() ->
		case mnesia:read(pools, Pool, write) of
		    [] ->
			Record = #pool{name=Pool, system=System},
			mnesia:write(pools, Record, write),
			{created, Record};
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {created, Record}} ->
	    {created, Record};
	{atomic, exists} ->
	    exists
    end.

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

handle_call({create, Pool}, _From, State) ->
    case create_pool(Pool, false) of
	{created, _Record} ->
	    {reply, ok, State};
	exists ->
	    {reply, exists, State}
    end;
handle_call(enum, _From, State) ->
    Pools = enum_i(),
    {reply, {ok, Pools}, State};
handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast({genevent_bridge, {system, discovery, {Mac, _Inventory}}}, State) ->
    ok = assign_to_pool(Mac, "Unassigned", unconfigured, State),
    {noreply, State};
handle_cast({genevent_bridge, Event}, State) ->
    {noreply, State};
handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

terminate(Reason, State) ->
    genevent_genserver_bridge:delete_handler(host_events, self()),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
