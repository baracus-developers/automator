-module(puppetca_mon).
-export([start_link/0]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

process({add, {Host, Record}}) ->		      
    gen_event:notify(machine_events,
		     {puppetca, add, Host, Record});
process({update, {Host, OldRecord, NewRecord}}) ->
    gen_event:notify(machine_events,
		     {puppetca, update, Host, OldRecord, NewRecord}).

poller() ->
    Delta = puppetca_driver:refresh(),
    lists:map(fun(I) -> process(I) end, Delta),
    timer:sleep(5000),
    poller().


