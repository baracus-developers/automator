-module(power_server).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("power.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_profile/1, delete_profile/1, enum_profiles/0, enum_nodes/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

init(_Args) ->
    genevent_bridge:add_genserver_handler(host_events, self(), self()),
    ok = util:open_table(powerprofiles,
			 [
			  {record_name, powerprofile},
			  {attributes,
			   record_info(fields, powerprofile)},
			  {disc_copies, util:replicas()}
			 ]),
    ok = util:open_table(powernodes,
			 [
			  {record_name, powernode},
			  {attributes,
			   record_info(fields, powernode)},
			  {disc_copies, util:replicas()}
			 ]),
    {ok, null}.

add_profile(Profile) when is_record(Profile, powerprofile) ->
    gen_server:call(?MODULE, {add_profile, Profile}).

delete_profile(Name) ->
    gen_server:call(?MODULE, {delete_profile, Name}).

enum_profiles() ->
    gen_server:call(?MODULE, enum_profiles).

enum_nodes() ->
    gen_server:call(?MODULE, enum_nodes).

enum_profiles_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(powerprofiles)])).

enum_nodes_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(powernodes)])).

handle_call({add_profile, Profile}, _From, State) ->
    F = fun() ->
		case mnesia:read(powerprofiles, Profile#powerprofile.name, write) of
		    [] ->
			mnesia:write(powerprofiles, Profile, write),
			created;
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, created} ->
	    gen_event:notify(host_events, {system, powerprofile, added, Profile}),
	    {reply, ok, State};
	{atomic, exists} ->
	    {reply, {error, conflict}, State}
    end;

handle_call({delete_profile, Name}, _From, State) ->
    F = fun() ->
		mnesia:delete(powerprofiles, Name, write)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events, {system, powerprofile, deleted, Name}),
	    {reply, ok, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call(enum_profiles, _From, State) ->
    {reply, {ok, enum_profiles_i()}, State};
handle_call(enum_nodes, _From, State) ->
    {reply, {ok, enum_nodes_i()}, State};
handle_call(_Request, _From, _State) ->
    throw(unexpected).

handle_cast({genevent_bridge, {system, discovery, {Mac, _Inventory}}}, State) ->
    F = fun() ->
		case mnesia:read(powernodes, Mac, write) of
		    [] ->
			Record = #powernode{mac=Mac},
			mnesia:write(powernodes, Record, write),
			created;
		    [Record] ->
			throw({exists, Mac})
		end
	end,
    {atomic, created} = mnesia:transaction(F),
    gen_event:notify(host_events, {system, powernode, added, Mac}),
    {noreply, State};

handle_cast({genevent_bridge, Event}, State) ->
    {noreply, State};
handle_cast(_Request, _State) ->
    throw(unexpected).

handle_info(_Info, _State) -> 
    throw(unexpected).

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


    
