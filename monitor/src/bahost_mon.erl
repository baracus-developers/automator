-module(bahost_mon).
-include_lib("bahost_record.hrl").
-export([start_link/0]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

process({add, {Mac, Record}}) ->		      
    gen_event:notify(host_events,
		     {baracus, add, Mac, Record});
process({update, {Mac, OldRecord, NewRecord}}) ->
    gen_event:notify(host_events,
		     {baracus, update, Mac, OldRecord, NewRecord}).

poller() ->
    Delta = baracus_driver:refresh(),
    lists:map(fun(I) -> process(I) end, Delta),
    timer:sleep(5000),
    poller().

