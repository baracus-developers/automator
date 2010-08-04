-module(bahost_analyzer).
-include_lib("bahost_record.hrl").
-export([start_link/0, analyze/1]).

test1() -> [ #bahost{mac = "00:0C:29:43:0B:FC",
		     pxecurr = localboot,
		     pxenext = localboot,
		     state = build,
		     active = enabled}].
test2() -> [ #bahost{mac = "00:0C:29:43:0B:FC",
		     pxecurr = localboot,
		     pxenext = localboot,
		     state = built,
		     active = enabled},
	     #bahost{mac = "00:0C:29:4D:1C:5B",
		     pxecurr = inventory,
		     pxenext = build,
		     state = build,
		     active = enabled}].

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> listener(dict:new()) end),
    register(bahost_analyzer, Pid),
    {ok, Pid}.

analyze(Data) ->
    bahost_analyzer ! Data.

listener(Dict) ->
    receive
	Data ->
	    NextDict = process(Dict, Data),
	    listener(NextDict)
    end.

process(Dict, [H | T]) ->
    Mac = H#bahost.mac,
    case dict:find(Mac, Dict) of
	{ok, Value} ->
	    if
		Value =/= H ->
		    gen_event:notify(host_events, {baracus, update, Mac, Value, H}),
		    process(dict:store(Mac, H, Dict), T);
		true ->
		    process(Dict, T)
	    end;
	error ->
	    gen_event:notify(host_events, {baracus, add, Mac, H}),
	    process(dict:store(Mac, H, Dict), T)
    end;
process(Dict, []) ->
    Dict.

