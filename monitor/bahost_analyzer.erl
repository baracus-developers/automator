-module(bahost_analyzer).
-include_lib("bahost_record.hrl").
-export([run_once/0, start_link/0, analyze/1]).

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

run_once() ->
    io:format("Initializing puppetca table~n"),
    {atomic, ok} = mnesia:create_table(bahost,
				       [
					{record_name, bahost},
					{attributes,
					 record_info(fields, bahost)},
					{disc_copies, [node()]}
				       ]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> listener_init() end),
    register(bahost_analyzer, Pid),
    {ok, Pid}.

analyze(Data) ->
    bahost_analyzer ! Data.

listener_init() ->
    ok = mnesia:wait_for_tables([bahost], 20000),
    listener().

listener() ->
    receive
	Data ->
	    process(Data),
	    listener()
    end.

process([H | T]) ->
    Mac = H#bahost.mac,
    F = fun() ->
		case mnesia:read({bahost, Mac}) of
		    [] ->
			mnesia:write(H),
			add;
		    [H] ->
			noop;
		    [Value] ->
			mnesia:write(H),
			{update, Value}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, add} ->
	    gen_event:notify(host_events, {baracus, add, Mac, H});
	{atomic, {update, Value}} ->
	    gen_event:notify(host_events, {baracus, update, Mac, Value, H});
	{atomic, noop} ->
	    void
    end,
    process(T);
process([]) ->
    void.

