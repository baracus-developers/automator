-module(bahost_mon).
-include_lib("bahost_record.hrl").
-export([start_link/0]).

-record(state, {zone}).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> init() end),
    {ok, Pid}.

process({add, {Mac, Record}}) ->		      
    gen_event:notify(host_events,
		     {baracus, add, Mac, Record});
process({update, {Mac, OldRecord, NewRecord}}) ->
    gen_event:notify(host_events,
		     {baracus, update, Mac, OldRecord, NewRecord}).

init() ->
    [_, Zone] = string:tokens(atom_to_list(node()), "@"),
    poller(#state{zone=Zone}).

poller(State) ->
    Delta = baracus_driver:refresh(State#state.zone),
    lists:map(fun(I) -> process(I) end, Delta),
    timer:sleep(5000),
    poller(State).

