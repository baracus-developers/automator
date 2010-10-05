-module(rules_server).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("staging.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add/2, delete/1, enum/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 


init(_Args) ->
    ok = util:open_table(stagingrules,
			 [
			  {record_name, stagingrule},
			  {attributes,
			   record_info(fields, stagingrule)},
			  {disc_copies, util:replicas()}
			 ]),

    {ok, null}.

add(Name, XPath) ->
    gen_server:call(?MODULE, {add, Name, XPath}).

delete(Name) ->
    gen_server:call(?MODULE, {delete, Name}).

enum() ->
    gen_server:call(?MODULE, enum).

handle_call({add, Name, XPath}, _From, State) ->
    F = fun() ->
		case mnesia:read(stagingrules, Name, write) of
		    [] ->
			Record = #stagingrule{name=Name, xpath=XPath},
			mnesia:write(stagingrules, Record, write),
			{created, Record};
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {created, Record}} ->
	    gen_event:notify(host_events, {system, stagingrule, added, Record}),
	    {reply, ok, State};
	{atomic, exists} ->
	    {reply, {error, conflict}, State}
    end;

handle_call({delete, Name}, _From, State) ->
    F = fun() ->
		mnesia:delete(stagingrules, Name, write)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events, {system, stagingrule, deleted, Name}),
	    {reply, ok, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call(enum, _From, State) ->
    StagingRules = util:atomic_query(qlc:q([X || X <- mnesia:table(stagingrules)])),
    {reply, {ok, StagingRules}, State};

handle_call(_Request, _From, _State) ->
    throw(unexpected).

handle_cast(_Request, _State) ->
    throw(unexpected).

handle_info(_Info, _State) -> 
    throw(unexpected).

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


    
