-module(puppetca_analyzer).
-include_lib("certentry.hrl").
-export([start_link/0, analyze/1]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> listener(dict:new()) end),
    register(puppetca_analyzer, Pid),
    {ok, Pid}.

analyze(Data) ->
    puppetca_analyzer ! Data.

listener(Dict) ->
    receive
	Data ->
	    NextDict = process(Dict, Data),
	    listener(NextDict)
    end.

process(Dict, [H | T]) ->
    Host = H#certentry.host,
    case dict:find(Host, Dict) of
	{ok, Value} ->
	    if
		Value =/= H ->
		    gen_event:notify(machine_events,
				     {puppetca, update, Host, Value, H}),
		    process(dict:store(Host, H, Dict), T);
		true ->
		    process(Dict, T)
	    end;
	error ->
	    gen_event:notify(machine_events, {puppetca, add, Host, H}),
	    process(dict:store(Host, H, Dict), T)
    end;
process(Dict, []) ->
    Dict.

