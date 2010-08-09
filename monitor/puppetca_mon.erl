-module(puppetca_mon).
-export([run_once/1, start_poller_link/0, start_delta_link/0]).

tablename() -> puppetca.

run_once(Nodes) ->
    io:format("Initializing puppetca table~n"),
    {atomic, ok} = delta_keyvalue:run_once(tablename(), Nodes).

start_poller_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

start_delta_link() ->
    delta_keyvalue:start_link(tablename()).
 
process({add, {Host, Record}}) ->		      
    gen_event:notify(machine_events,
		     {puppetca, add, Host, Record});
process({update, {Host, OldRecord, NewRecord}}) ->
    gen_event:notify(machine_events,
		     {puppetca, update, Host, OldRecord, NewRecord}).

poller() ->
    RawData = os:cmd("puppetca -l --all"),
    Data = puppetca_parser:process(RawData),
    Delta = delta_keyvalue:analyze(tablename(), Data),
    lists:map(fun(I) -> process(I) end, Delta),
    timer:sleep(5000),
    poller().


