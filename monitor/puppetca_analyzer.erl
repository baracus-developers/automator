-module(puppetca_analyzer).
-include_lib("certentry.hrl").
-export([run_once/0, start_link/0, analyze/1]).

run_once() ->
    io:format("Initializing puppetca table~n"),
    {atomic, ok} = mnesia:create_table(puppetca,
				       [
					{record_name, certentry},
					{attributes,
					 record_info(fields, certentry)},
					{disc_copies, [node()]}
				       ]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> listener_init() end),
    register(puppetca_analyzer, Pid),
    %spawn(fun() -> tracer_init(Pid) end),
    {ok, Pid}.

tracer_init(Pid) ->
    erlang:trace(Pid, true, [all]),
    tracer().

tracer() ->
    receive
	Trace ->
	    io:format("TRACE: ~p~n", [Trace])
    end,
    tracer().

analyze(Data) ->
    puppetca_analyzer ! Data.

listener_init() ->
    ok = mnesia:wait_for_tables([puppetca], 20000),
    listener().

listener() ->
    receive
	Data ->
	    process(Data),
	    listener()
    end.

process([H | T]) ->
    Host = H#certentry.host,
    F = fun() ->
		case mnesia:read({puppetca, Host}) of
		    [] ->
			mnesia:write(puppetca, H, write),
			add;
		    [H] ->
			noop;
		    [Value] ->
			mnesia:write(puppetca, H, write),
			{update, Value}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, add} ->
	    gen_event:notify(machine_events, {puppetca, add, Host, H});
	{atomic, {update, Value}} ->
	    gen_event:notify(machine_events,
			     {puppetca, update, Host, Value, H});	
	{atomic, noop} ->
	    void
    end,
    process(T);
process([]) ->
    void.


