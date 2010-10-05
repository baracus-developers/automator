-module(staging_server).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include("staging.hrl").
-include("baracus.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_rule/2, delete_rule/1, enum_rules/0]).
-export([add_profile/1, delete_profile/1, enum_profiles/0]).
-export([enum_nodes/0, set_param/3, deploy_node/1, reject_node/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

init(_Args) ->
    genevent_bridge:add_genserver_handler(host_events, self(), self()),
    ok = util:open_table(stagingrules,
			 [
			  {record_name, stagingrule},
			  {attributes,
			   record_info(fields, stagingrule)},
			  {disc_copies, util:replicas()}
			 ]),
    ok = util:open_table(stagingprofiles,
			 [
			  {record_name, stagingprofile},
			  {attributes,
			   record_info(fields, stagingprofile)},
			  {disc_copies, util:replicas()}
			 ]),
    ok = util:open_table(stagingnodes,
			 [
			  {record_name, stagingnode},
			  {attributes,
			   record_info(fields, stagingnode)},
			  {disc_copies, util:replicas()}
			 ]),
    {ok, null}.

add_rule(Name, XPath) ->
    gen_server:call(?MODULE, {add_rule, Name, XPath}).

delete_rule(Name) ->
    gen_server:call(?MODULE, {delete_rule, Name}).

enum_rules() ->
    gen_server:call(?MODULE, enum_rules).

add_profile(Profile) when is_record(Profile, stagingprofile) ->
    gen_server:call(?MODULE, {add_profile, Profile}).

delete_profile(Name) ->
    gen_server:call(?MODULE, {delete_profile, Name}).

set_param(Mac, Param, Value) ->
    gen_server:call(?MODULE, {set_param, Mac, Param, Value}).

deploy_node(Mac) ->
    gen_server:call(?MODULE, {deploy_node, Mac}).

reject_node(Mac) ->
    gen_server:call(?MODULE, {reject_node, Mac}).

enum_profiles() ->
    gen_server:call(?MODULE, enum_profiles).

enum_nodes() ->
    gen_server:call(?MODULE, enum_nodes).

enum_profiles_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(stagingprofiles)])).

enum_nodes_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(stagingnodes)])).

update_stagingnode(Record, host, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{host=Value};
update_stagingnode(Record, type, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{type=Value};
update_stagingnode(Record, bmcaddr, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{bmcaddr=Value};
update_stagingnode(Record, username, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{username=Value};
update_stagingnode(Record, password, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{password=Value}.

handle_call({add_profile, Profile}, _From, State) ->
    F = fun() ->
		case mnesia:read(stagingprofiles, Profile#stagingprofile.name, write) of
		    [] ->
			mnesia:write(stagingprofiles, Profile, write),
			created;
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, created} ->
	    gen_event:notify(host_events, {system, stagingprofile, added, Profile}),
	    {reply, ok, State};
	{atomic, exists} ->
	    {reply, {error, conflict}, State}
    end;

handle_call({delete_profile, Name}, _From, State) ->
    F = fun() ->
		mnesia:delete(stagingprofiles, Name, write)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events, {system, stagingprofile, deleted, Name}),
	    {reply, ok, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call(enum_profiles, _From, State) ->
    {reply, {ok, enum_profiles_i()}, State};
handle_call(enum_nodes, _From, State) ->
    {reply, {ok, enum_nodes_i()}, State};
handle_call({set_param, Mac, Param, Value}, _From, State) ->
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Record] ->
			UpdatedRecord = update_stagingnode(Record, Param, Value),
			mnesia:write(stagingnodes, UpdatedRecord, write),
			updated
		end
	end,
    case mnesia:transaction(F) of
	{atomic, updated} ->
	    gen_event:notify(host_events, {system, stagingnode, updated, Mac}),
	    {reply, ok, State};
	{atomic, noexists} ->
	    {reply, {error, noexists}, State}
    end;

handle_call({deploy_node, Mac}, _From, State) ->
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Record] ->
			PowerConfig = #powerconfig{mac=Mac,
						   type=Record#stagingnode.type,
						   host=Record#stagingnode.host,
						   username=Record#stagingnode.username,
						   password=Record#stagingnode.password,
						   bmcaddr=Record#stagingnode.bmcaddr
						  },
			case host_fsm:configure_power(Mac, PowerConfig) of
			    ok ->
				% FIXME: We can still roll back this transaction even if 
				% baracus was updated and miss this delete.  We need to handle
				% this case
				mnesia:delete(stagingnodes, Mac, write);
			    {error, Error} -> Error;
			    Else -> Else
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events, {system, stagingnode, deployed, Mac}),
	    {reply, ok, State};
	{atomic, Error} ->
	    {reply, {error, Error}, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({reject_node, Mac}, _From, State) ->
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Record] ->
			mnesia:delete(stagingnodes, Mac, write)
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events, {system, stagingnode, rejected, Mac}),
	    {reply, ok, State};
	{atomic, Error} ->
	    {reply, {error, Error}, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({add_rule, Name, XPath}, _From, State) ->
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

handle_call({delete_rule, Name}, _From, State) ->
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

handle_call(enum_rules, _From, State) ->
    StagingRules = util:atomic_query(qlc:q([X || X <- mnesia:table(stagingrules)])),
    {reply, {ok, StagingRules}, State};
  
handle_call(_Request, _From, _State) ->
    throw(unexpected).

handle_cast({genevent_bridge, {system, discovery, {Mac, _Inventory}}}, State) ->
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			Record = #stagingnode{mac=Mac},
			mnesia:write(stagingnodes, Record, write),
			created;
		    [Record] ->
			throw({exists, Mac})
		end
	end,
    {atomic, created} = mnesia:transaction(F),
    gen_event:notify(host_events, {system, stagingnode, added, Mac}),
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


    
