-module(delta_keyvalue).
-behavior(gen_server).
-compile(export_all).

-record(deltakv, {key, value}).

run_once(Table, Nodes) ->
    {atomic, ok} = mnesia:create_table(Table,
				       [
					{record_name, deltakv},
					{attributes,
					 record_info(fields, deltakv)},
					{disc_copies, Nodes}
				       ]).

start_link(Table) ->
    gen_server:start_link({local, Table}, delta_keyvalue, Table, []).

init(Table) ->
    ok = mnesia:wait_for_tables([Table], 20000),
    {ok, Table}.

analyze(Table, Data) ->
    gen_server:call(Table, {analyze, Data}).

find(Table, Key) ->
    gen_server:call(Table, {find, Key}).

handle_call({analyze, Data}, _From, Table) ->
    ProcessedData = lists:map(fun(I) -> process(Table, I) end, Data),
    {reply, [ X || X <- ProcessedData, X =/= void], Table};
handle_call({find, Key}, _From, Table) ->
    F = fun() ->
		 mnesia:read(Table, Key, read)
	end,
    case mnesia:transaction(F) of
	{atomic, []} ->
	    {reply, notfound, Table};
	{atomic, [Record]} ->
	    {reply, {ok, Record#deltakv.value}, Table}
    end.

process(Table, {Key, Value}) ->
    F = fun() ->
		Record = #deltakv{key = Key, value = Value},
		case mnesia:read(Table, Key, write) of
		    [] ->
			mnesia:write(Table, Record, write),
			add;
		    [Record] ->
			noop;
		    [OldValue] ->
			mnesia:write(Table, Record, write),
			{update, OldValue#deltakv.value}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, add} ->
	    {add, {Key, Value}};
	{atomic, {update, OldValue}} ->
	    {update, {Key, OldValue, Value}};
	{atomic, noop} ->
	    void
    end.
