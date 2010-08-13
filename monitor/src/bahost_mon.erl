-module(bahost_mon).
-include_lib("bahost_record.hrl").
-export([run_once/1, start_poller_link/0, start_delta_link/0, get_state/1]).

tablename() -> bahost.

run_once(Nodes) ->
    io:format("Initializing bahost table~n"),
    {atomic, ok} = delta_keyvalue:run_once(tablename(), Nodes).

start_poller_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

start_delta_link() ->
    delta_keyvalue:start_link(tablename()).
 
process({add, {Mac, Record}}) ->		      
    gen_event:notify(host_events,
		     {baracus, add, Mac, Record});
process({update, {Mac, OldRecord, NewRecord}}) ->
    gen_event:notify(host_events,
		     {baracus, update, Mac, OldRecord, NewRecord}).

poller() ->
    RawData = os:cmd("bahost list states"),
    Data = bahost_parser:process(RawData),
    Delta = delta_keyvalue:analyze(tablename(), Data),
    lists:map(fun(I) -> process(I) end, Delta),
    timer:sleep(5000),
    poller().

get_state(Mac) ->
    case delta_keyvalue:find(tablename(), Mac) of
	notfound ->
	    notfound;
	{ok, Record} ->
	    {ok, Record#bahost.state}
    end.
