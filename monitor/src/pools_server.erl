-module(pools_server).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-export([start_link/0, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/1, enum/0, enum_members/1]).

-record(state, {}).

-record(member, {mac, pool, category}).
-record(pool, {name, system, created_on}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    ok = util:open_table(members,
			 [
			  {record_name, member},
			  {attributes,
			   record_info(fields, member)},
			  {disc_copies, util:replicas()}
			 ]),
    ok = util:open_table(pools,
			 [
			  {record_name, pool},
			  {attributes,
			   record_info(fields, pool)},
			  {disc_copies, util:replicas()}
			 ]),

    atomic_create_pool(default_pool(), true),

    genevent_bridge:add_genserver_handler(host_events, self(), self()),
    {ok, #state{}}.

create(Pool) ->
    gen_server:call(?MODULE, {create, Pool}).

enum() ->
    gen_server:call(?MODULE, enum).

enum_members(Pool) ->
    gen_server:call(?MODULE, {enum_members, Pool}).

enum_i() ->
    util:atomic_query(qlc:q([X#pool.name || X <- mnesia:table(pools)])).

assign_to_pool(Mac, Pool, Category) ->
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

    gen_event:notify(pool_events,
		     {system, pool_assignment, Mac, Pool, Category}),
    
    ok.

create_pool(Pool, System) ->
    case mnesia:read(pools, Pool, write) of
	[] ->
	    Record = #pool{name=Pool, system=System},
	    mnesia:write(pools, Record, write),
	    {created, Record};
	[Record] ->
	    exists
    end.    

atomic_create_pool(Pool, System) ->
    case mnesia:transaction(fun() -> create_pool(Pool, System) end) of
	{atomic, Result} -> Result
    end.

handle_call({create, Pool}, _From, State) ->
    case atomic_create_pool(Pool, false) of
	{created, _Record} ->
	    {reply, ok, State};
	exists ->
	    {reply, exists, State}
    end;
handle_call(enum, _From, State) ->
    Pools = enum_i(),
    {reply, {ok, Pools}, State};
handle_call({enum_members, {Pool, Category}}, _From, State) ->
    Members = util:atomic_query(qlc:q([X#member.mac || 
					  X <- mnesia:table(members),
					  X#member.pool == Pool,
					  X#member.category == Category
				      ]
				     )
			       ),
    {reply, {ok, Members}, State};
handle_call({enum_members, Pool}, _From, State) ->
    Members = util:atomic_query(qlc:q([{X#member.mac, X#member.category} ||
					  X <- mnesia:table(members),
					  X#member.pool == Pool
				      ]
				     )
			       ),
    {reply, {ok, Members}, State};
handle_call(Request, From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast({genevent_bridge,
	     {Principal, stagingnode, deployed, {Mac, P}}},
	    State) ->

    Pool = case P of
	       undefined -> default_pool();
	       _ -> P
	   end,
    
    F = fun() ->
		case create_pool(Pool, false) of
		    {created, _} -> ok;
		    exists -> ok
		end,

		ok = assign_to_pool(Mac, Pool, free)
	end,
    {atomic, ok} = mnesia:transaction(F),
    {noreply, State};
handle_cast({genevent_bridge, Event}, State) ->
    {noreply, State};
handle_cast(Request, State) ->
    throw(unexpected).

handle_info(Info, State) ->
    throw(unexpected).

terminate(Reason, State) ->
    genevent_bridge:delete_handler(host_events, self()),
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
default_pool() -> "Default".
