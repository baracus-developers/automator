-module(service_element_fsm).
-behavior(gen_fsm).
-export([init/1]).
-compile(export_all).

-record(state, {id, type, pools, needs, has=0}).

init({Id, Type, Pools, Elasticity}) ->

    Callback = fun(Mac) ->
		       gen_fsm:sync_send_event(Id, {inventory, Mac})
	       end,

    ok = scheduler:schedule(Id, Pools, Elasticity, Callback),

    State = #state{id=Id,
		   type=Type,
		   pools=Pools,
		   needs=Elasticity
		  },

    {ok, acquire_inventory, State}.

acquire_inventory({inventory, Mac}, From, State) ->
    io:format("Mac: ~p acquired~n", [Mac]),
    {reply, true, acquire_inventory, State#state{has=State#state.has+1}}.    

terminate(Reason, StateName, StateData) ->
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

    
