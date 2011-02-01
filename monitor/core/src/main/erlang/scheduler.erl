-module(scheduler).
-behavior(gen_server).
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export([schedule/4]).

-record(subscriber, {id, pools, needs, callback}).
-record(pool, {pool, inventory=sets:new(), subscribers=sets:new()}).
-record(state, {pools=dict:new(), subscribers=dict:new()}).

start_link() ->
    start_link(production).

start_link(Mode) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, Mode, []).

init(production) ->
    genevent_bridge:add_genserver_handler(pool_events, self(), self()),

%    {ok, PoolsList} = pools_server:enum(),
%    Pools = [{Pool, #pool{pool=Pool, inventory=get_members(Pool)}} 
%		|| Pool <- PoolsList],
    Pools = [],
   
    {ok, #state{pools=dict:from_list(Pools)}};

init(test) ->
    {ok, #state{}}.

schedule(Id, Pools, Needs, Callback) ->
    gen_server:call({global, ?MODULE},
		    {schedule, Id, Pools, Needs, Callback}).

handle_call({schedule, Id, Pools, Needs, Callback}, From, State) -> 
    UpdatedState = schedule(Id, Pools, Needs, Callback, State),

    % send the reply now since flush_pool() may invoke callbacks
    gen_server:reply(From, ok),
    {noreply, flush_pools(UpdatedState, Pools)}. 

handle_cast({genevent_bridge, {_, pool_assignment, Mac, PoolId, free}},
	    State) ->
    UpdatedState = add_inventory(PoolId, Mac, State),
    {noreply, flush_pool(UpdatedState, PoolId)};

handle_cast({genevent_bridge, _}, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

schedule(Id, Pools, Needs, Callback, State) ->
    Subscriber = #subscriber{id=Id,
			     pools=sets:from_list(Pools),
			     needs=Needs,
			     callback=Callback},
    Subscribers = dict:store(Id, Subscriber, State#state.subscribers),
    subscribe(State#state{subscribers=Subscribers}, Id, Pools).
    
get_members(Pool) ->
    {ok, Members} = pools_server:enum_members(Pool),
    sets:from_list([Mac || {Mac, free} <- Members]).

flush_pools(State, all) ->
    Pools = [Pool || {Pool, _} <- dict:to_list(State#state.pools)],
    flush_pools(State, Pools);
flush_pools(State, [PoolId | T]) ->
    UpdatedState = flush_pool(State, PoolId),
    flush_pools(UpdatedState, T);
flush_pools(State, []) ->
    State.

flush_pool(State, PoolId) ->
    case dict:find(PoolId, State#state.pools) of
	{ok, Pool} ->
	    Fold = fun(Mac, State) ->
			   % reload the pool, since the state may have changed
			   Pool = dict:fetch(PoolId, State#state.pools),
			   Subscribers = sets:to_list(Pool#pool.subscribers),
			   flush_node(State, PoolId, Mac, Subscribers)
		   end,
	    sets:fold(Fold, State, Pool#pool.inventory);
	_ -> State
    end.

flush_node(State, PoolId, Mac, [SubscriberId | T]) ->
    Subscriber = dict:fetch(SubscriberId, State#state.subscribers),
    Callback = Subscriber#subscriber.callback,
    case Callback(Mac) of
	true ->
	    Update = fun(Pool) ->
			     I = sets:del_element(Mac, Pool#pool.inventory),
			     Pool#pool{inventory=I}
		     end,
	    
	    Pools = dict:update(PoolId, Update, State#state.pools),
	    decrement_subscription(State#state{pools=Pools}, Subscriber);
	false ->
	    flush_node(State, PoolId, Mac, T)
    end;
flush_node(State, _PoolId, _Mac, []) ->
    State.

decrement_subscription(State, Subscriber=#subscriber{needs=1}) ->
    Id = Subscriber#subscriber.id,
    F = fun(PoolId, State) ->
		Update = fun(Pool) ->
				 S = sets:del_element(Id, Pool#pool.subscribers),
				 Pool#pool{subscribers=S}
			 end,
				 
		Pools = dict:update(PoolId, Update, State#state.pools),
		State#state{pools=Pools}
	end,
    State1 = sets:fold(F, State, Subscriber#subscriber.pools),
    Subscribers = dict:erase(Id, State1#state.subscribers),
    State1#state{subscribers=Subscribers};
decrement_subscription(State, Subscriber) ->
    Id = Subscriber#subscriber.id,
    F = fun(S) -> S#subscriber{needs=S#subscriber.needs-1} end,
    Subscribers = dict:update(Id, F, State#state.subscribers),
    State#state{subscribers=Subscribers}.

subscribe(State, Id, [PoolId | T]) ->
    Update = fun(Pool) ->
		     Subscribers = sets:add_element(Id, Pool#pool.subscribers),
		     Pool#pool{subscribers=Subscribers}
	     end,
    
    IV = #pool{pool=PoolId, subscribers=sets:from_list([Id])},
    Pools = dict:update(PoolId, Update, IV , State#state.pools),

    subscribe(State#state{pools=Pools}, Id, T);

subscribe(State, Id, []) ->
    State.
  
add_inventory(PoolId, Mac, State) ->
    Update = fun(Pool) ->
		     I = sets:add_element(Mac, Pool#pool.inventory),
		     Pool#pool{inventory=I}
	     end,
    
    IV = #pool{pool=PoolId, inventory=sets:from_list([Mac])},
    Pools = dict:update(PoolId, Update, IV, State#state.pools),

    State#state{pools=Pools}.
  
%---------------------------------------------------------------------------
% unit tests
%---------------------------------------------------------------------------

tracer() ->
    receive
	{flush, From} ->
	    From ! flushed,
	    exit(normal);
	Msg ->
	    ?debugFmt("TRACE: ~p~n", [Msg])
    end,
    tracer().

subscribe_test() ->
    State = subscribe(#state{}, "Me", ["Default"]),
    Pool = dict:fetch("Default", State#state.pools),
    true = sets:is_element("Me", Pool#pool.subscribers).

inventory_test() ->
    Mac = "00:11:22:33:44:55",
    State = add_inventory("Default", Mac, #state{}),
    Pool = dict:fetch("Default", State#state.pools),
    true = sets:is_element(Mac, Pool#pool.inventory).

verify_needs(State, Id, Needs) ->
    Subscriber = dict:fetch(Id, State#state.subscribers),
    case Subscriber#subscriber.needs of
	Needs -> ok;
	Val -> throw({illegal_needs, Val})
    end.

flush_test() ->
    Id = "Me",
    PoolId = "Default",
    Mac1 = "00:11:22:33:44:51",
    Mac2 = "00:11:22:33:44:52",
						%Mac = "00:11:22:33:44:55",
    S = self(),
    
    Callback = fun(Mac) ->
		       S ! {inventory, Mac},
		       true
	       end,
    
    State = schedule(Id, [PoolId], 2, Callback, #state{}),
    Subscriber = dict:fetch(Id, State#state.subscribers),
    true = sets:is_element(PoolId, Subscriber#subscriber.pools),
    verify_needs(State, Id, 2),
    State2 = add_inventory(PoolId, Mac1, State),
    State3 = flush_node(State2, PoolId, Mac1, [Id]),
    verify_needs(State3, Id, 1),
    State4 = add_inventory(PoolId, Mac2, State3),
    State5 = flush_node(State4, PoolId, Mac2, [Id]),
    error = dict:find(Id, State5#state.subscribers).
    
scheduler_test() ->
    {ok, Pid} = start_link(test),

%    Tracer = spawn(fun() -> tracer() end),
%    erlang:trace(Pid, true, [call, {tracer, Tracer}]),
%    erlang:trace_pattern({?MODULE, '_', '_'}, true, [local]),
    
    S = self(),
    PoolId = "Default",
    Mac = "00:11:22:33:44:55",

    Callback = fun(Mac) ->
		       S ! {callback, Mac},
		       true
	       end,

    ok = schedule("Me", [PoolId], 2, Callback),
    ok = gen_server:cast({global, ?MODULE}, {genevent_bridge,
					     {system, pool_assignment,
					      Mac, PoolId, free}}),
    
    receive
	{callback, Mac} -> ok
    after 500 ->
	    throw(timeout)
    end,

%    Tracer ! {flush, self()},
%    receive
%	flushed -> ok
%    end,
	
    ok.
