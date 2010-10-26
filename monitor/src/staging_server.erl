-module(staging_server).
-behavior(gen_server).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("staging.hrl").
-include("baracus.hrl").

-export([init/1, start_link/0, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add_rule/5, delete_rule/1, enum_rules/0, order_rules/1]).
-export([add_profile/1, delete_profile/1, enum_profiles/0]).
-export([add_resolver/2, delete_resolver/1, enum_resolvers/0]).
-export([enum_nodes/0, set_param/3, deploy_node/1, reject_node/1]).

-export([start_discovery/1]).

resolvers_path() -> "/var/cloudbuilder/resolvers/".
pc_selector() -> "//node[@id=\"powercontroller\"]/configuration/setting".

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []). 

init(_Args) ->
    random:seed(now()),
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
    ok = util:open_table(resolvers,
			 [
			  {record_name, resolver},
			  {attributes,
			   record_info(fields, resolver)},
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

add_rule(Name, XPath, Profile, Resolver, Action) ->
    gen_server:call(?MODULE, {add_rule, Name, XPath, Profile, Resolver, Action}).

delete_rule(Name) ->
    gen_server:call(?MODULE, {delete_rule, Name}).

enum_rules() ->
    gen_server:call(?MODULE, enum_rules).

order_rules(OrderList) ->
    gen_server:call(?MODULE, {order_rules, OrderList}).

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

add_resolver(FileName, LocalFileName) ->
    gen_server:call(?MODULE, {add_resolver, FileName, LocalFileName}).

delete_resolver(Id) ->
    gen_server:call(?MODULE, {delete_resolver, Id}).

enum_resolvers() ->
    gen_server:call(?MODULE, enum_resolvers).

enum_profiles_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(stagingprofiles)])).

enum_nodes_i() ->
    util:atomic_query(qlc:q([X || X <- mnesia:table(stagingnodes)])).

update_stagingnode(Record, pool, Value) when is_record(Record, stagingnode) ->
    Record#stagingnode{pool=Value};
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

max(Lhs, Rhs) when Lhs < Rhs ->
    Rhs;
max(Lhs, _) ->
    Lhs.

get_next_priority() ->
    Handle = qlc:q([X#stagingrule.priority || X <- mnesia:table(stagingrules)]),
    qlc:fold(fun(Answer, Acc) -> max(Answer, Acc) end, 0, Handle).

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
    Principal = {user, "Anonymous"},
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Record] ->
			UpdatedRecord = update_stagingnode(Record, Param, Value),
			mnesia:write(stagingnodes, UpdatedRecord, write),
			{updated, Record, UpdatedRecord}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {updated, Old, New}} ->
	    gen_event:notify(host_events, {Principal, stagingnode, updated, {Mac, Old, New}}),
	    {reply, ok, State};
	{atomic, noexists} ->
	    {reply, {error, noexists}, State}
    end;

handle_call({deploy_node, Mac}, _From, State) ->
    Principal = {user, "Anonymous"},
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Node] ->
			case deploy(Principal, Node) of
			    ok ->
			        % FIXME: We can still roll back this transaction
				% even if baracus was updated and miss this
				% delete.  We need to handle this case
				mnesia:delete(stagingnodes, Mac, write);
			    {error, Error} -> Error;
			    Else -> Else
			end
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    gen_event:notify(host_events,
			     {Principal, stagingnode, removed, Mac}),
	    {reply, ok, State};
	{atomic, Error} ->
	    {reply, {error, Error}, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({reject_node, Mac}, _From, State) -> 
    Principal = {user, "Anonymous"},
    F = fun() ->
		case mnesia:read(stagingnodes, Mac, write) of
		    [] ->
			noexists;
		    [Record] ->
			mnesia:delete(stagingnodes, Mac, write),
			{ok, Record}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {ok, Node}} ->
	    ok = reject(Principal, Node),
	    gen_event:notify(host_events,
			     {Principal, stagingnode, removed, Mac}),
	    {reply, ok, State};
	{atomic, Error} ->
	    {reply, {error, Error}, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({add_rule, Name, XPath, Profile, Resolver, Action}, _From, State) ->
    F = fun() ->
		validate_ref(stagingprofiles, Profile),
		validate_ref(resolvers, Resolver),

		case mnesia:read(stagingrules, Name, write) of
		    [] ->
			Priority = get_next_priority(),
			Record = #stagingrule{name=Name,
					      priority=Priority+1,
					      xpath=XPath,
					      profile=Profile,
					      resolver=Resolver,
					      action=Action},
			mnesia:write(stagingrules, Record, write),
			{created, Record};
		    [Record] -> exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {created, Record}} ->
	    gen_event:notify(host_events, {system, stagingrule, added, Record}),
	    {reply, ok, State};
	{aborted, {throw, Error}} ->
	    {reply, {error, Error}, State};
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
    StagingRules = enum_rules_i(),
    {reply, {ok, StagingRules}, State};

handle_call({order_rules, OrderList}, _From, State) ->
    PrioFun = fun(Elem, {Prio, List}) ->
		      {Prio + 1, [{Prio, Elem} | List]}
	      end,
    {_, PrioList} = lists:foldl(PrioFun, {1, []}, OrderList),
    F = fun() ->
		[update_prio(Rule, Prio) || {Prio, Rule} <- PrioList],
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    gen_event:notify(host_events, {system, stagingrule, reordered, OrderList}),
    {reply, ok, State};
  
handle_call({add_resolver, Name, SourceName}, _From, State) ->
    Principal = "Anonymous",

    F = fun() ->
		Q = qlc:q([X#resolver.version
			   || X <- mnesia:table(resolvers),
			      X#resolver.name == Name]),
		Versions = qlc:e(Q),

		Max = fun(E, Acc) when E > Acc -> E;
			 (E, Acc) -> Acc
		      end,

		MaxVersion = lists:foldl(Max, 0, Versions),

		Id = uuid:to_string(uuid:v4()),
		Size = filelib:file_size(SourceName),
		DestName = id_to_filename(Id),
		
		Resolver = #resolver{id=Id,
				     name=Name,
				     version=MaxVersion+1,
				     uploaded=erlang:universaltime(),
				     owner=Principal,
				     size=Size
				    },
		
		ok = mnesia:write(resolvers, Resolver, write),
		
		{ok, Size} = file:copy(SourceName, DestName),
		ok = file:change_mode(DestName, 8#00500),
		ok = file:delete(SourceName),
		
		try util:os_cmd_format("~s --test", [DestName])
		catch
		    throw:_ ->
			file:delete(DestName),
			throw(badscript)
		end,

		{added, Id}
	end,
    case mnesia:transaction(F) of
	{atomic, {added, Id}} ->
	    gen_event:notify(host_events, {{user, Principal},
					   resolver, added, Id}),   
	    {reply, ok, State};
	{aborted, Error} ->
	    {reply, Error, State}
    end;

handle_call({delete_resolver, Id}, _From, State) ->
    Principal = "Anonymous",

    F = fun() ->
		case mnesia:read(resolvers, Id, write) of
		    [] ->
			not_found;
		    [Record] ->
			ok = mnesia:delete(resolvers, Id, write),
			ok = file:delete(id_to_filename(Id)),

			ok
		end
	end,
    case mnesia:transaction(F) of
	{atomic, not_found} ->
	    {reply, not_found, State};
	{atomic, ok} ->
	    gen_event:notify(host_events, {{user, Principal},
					   resolver, deleted, Id}),   
   
	    {reply, ok, State}
    end;

handle_call(enum_resolvers, _From, State) ->
    Resolvers = util:atomic_query(qlc:q([X || X <- mnesia:table(resolvers)])),

    Sort = fun(LHS, RHS) ->
		   if
		       LHS#resolver.name < RHS#resolver.name -> true;
		       LHS#resolver.name =:= RHS#resolver.name,
		       LHS#resolver.version =< RHS#resolver.version -> true;
		       true -> false
		   end
	   end,

    {reply, {ok, lists:sort(Sort, Resolvers)}, State};

handle_call(_Request, _From, _State) ->
    throw(unexpected).

handle_cast({genevent_bridge, {system, discovery, Mac}}, State) ->
    {ok, _} = supervisor:start_child(staging_sup,
				     {Mac,
				      {?MODULE, start_discovery, [Mac]},
				      transient,
				      brutal_kill,
				      worker,
				      [?MODULE]}),  

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

update_prio(Rule, Prio) ->
    case mnesia:read(stagingrules, Rule, write) of
	[] ->
	    throw({"Rule does not exist", Rule});
	[Record] when Record#stagingrule.priority =/= Prio ->
	    mnesia:write(stagingrules, Record#stagingrule{priority=Prio}, write);
	[Record] ->
	    ok
    end.

profile_decode([{_, undefined} | T], Node) ->
    profile_decode(T, Node);
profile_decode([{pool, Val} | T], Node) ->
    Inventory = insert_globalsetting("pool", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{pool=Val, inventory=Inventory});
profile_decode([{type, Val} | T], Node) ->
    Inventory = insert_powersetting("type", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{type=Val, inventory=Inventory});
profile_decode([{host, Val} | T], Node) ->
    Inventory = insert_powersetting("host", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{host=Val, inventory=Inventory});
profile_decode([{username, Val} | T], Node) ->
    Inventory = insert_powersetting("username", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{username=Val, inventory=Inventory});
profile_decode([{password, Val} | T], Node) ->
    Inventory = insert_powersetting("password", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{password=Val, inventory=Inventory});
profile_decode([{bmcaddr, Val} | T], Node) ->
    Inventory = insert_powersetting("bmcaddr", Val, Node#stagingnode.inventory),
    profile_decode(T, Node#stagingnode{bmcaddr=Val, inventory=Inventory});
profile_decode([{_, _Val} | T], Node) ->
    profile_decode(T, Node);
profile_decode([], Node) ->
    Node.

process_rule(Node, profile, Rule, T) when Rule#stagingrule.profile =/= undefined ->
    [Profile] = mnesia:read(stagingprofiles, Rule#stagingrule.profile, read),
    [_ | RawParams] = tuple_to_list(Profile),
    Fields = record_info(fields, stagingprofile),
    Params = lists:zip(Fields, RawParams),
    
    UpdatedNode = profile_decode(Params, Node),
    gen_event:notify(host_events,
		     {{rule, Rule#stagingrule.name}, stagingnode, updated,
		      {Node#stagingnode.mac, Node, UpdatedNode}}),

    process_rule(UpdatedNode, resolver, Rule, T);
process_rule(Node, profile, Rule, T) ->
    process_rule(Node, resolver, Rule, T);

process_rule(Node, resolver, Rule, T) when Rule#stagingrule.resolver =/= undefined ->
    Inventory = lists:flatten(
		  xmerl:export_element(Node#stagingnode.inventory,
				       xmerl_xml
				      )
		 ),
    
    Cmd = lists:flatten(io_lib:format("~s --process ~p",
				      [id_to_filename(Rule#stagingrule.resolver),
				       length(Inventory)
				      ]
				     )
		       ),

    R = try
	    Port = erlang:open_port({spawn, Cmd}, [use_stdio, stream, exit_status]),
	    
	    true = port_command(Port, Inventory),
	    
	    Output = case cmd_receive(Port, "") of
			 {ok, 0, Data} -> Data;
			 {ok, UnexpectedStatus, _Output} ->
			     throw({resolver_failure, UnexpectedStatus})
		     end,
	
	    {UpdatedInventory, _} = xmerl_scan:string(Output),

	    NewNode = 
		case xmerl_xs:select(pc_selector(), UpdatedInventory) of
		    [] -> Node#stagingnode{inventory=UpdatedInventory};
		    Elems ->
			ValidTypes = sets:from_list([atom_to_list(Type) ||
							Type <- baracus_driver:get_bmctypes()]),

			F = fun(E, Acc) ->
				    Key = select_attribute("@id", E),
				    Value = select_attribute("@value", E),
				    
				    case Key of
					"host" -> Acc#stagingnode{host=Value};
					"bmcaddr" -> Acc#stagingnode{bmcaddr=Value};
					"type" ->
					    case sets:is_element(Value, ValidTypes) of
						true -> Acc#stagingnode{type=Value};
						false -> throw({invalidtype, Value})
					    end;
					"username" -> Acc#stagingnode{username=Value};
					"password" -> Acc#stagingnode{password=Value};
					_ -> throw({badsetting, {Key, Value}})
				    end
			    end,
			lists:foldl(F,
				    Node#stagingnode{inventory=UpdatedInventory},
				    Elems)
		end,
	    
	    {continue, NewNode}
	catch
	    Type:Error ->
		error_logger:warning_msg(
		  "Resolver ~s(~s): Error ~p:~p ~n",
		  [Rule#stagingrule.resolver,
		   Node#stagingnode.mac,
		   Type, Error]), 
		terminate
	end,
			
    % check to see if we should continue processing this rule, or terminate it
    % it and move on to the next one (if applicable)
    case R of
	{continue, UpdatedNode} ->
	    process_rule(UpdatedNode, action, Rule, T);
	terminate ->
	    process_rules(Node, T)
    end;

process_rule(Node, resolver, Rule, T) ->
    process_rule(Node, action, Rule, T);

process_rule(Node, action, Rule, T) when Rule#stagingrule.action =:= "deploy" ->
    case deploy({rule, Rule#stagingrule.name}, Node) of
	ok -> ok;
	_ -> process_rules(Node, T)
    end;
process_rule(Node, action, Rule, T) when Rule#stagingrule.action =:= "reject" ->
    case reject({rule, Rule#stagingrule.name}, Node) of
	ok -> ok;
	_ -> process_rules(Node, T)
    end;
process_rule(Node, action, Rule, T) ->
    error_logger:info_msg("Moving to next rule due to action=~p~n",
			  [Rule#stagingrule.action]),
    process_rules(Node, T).

process_rules(Node, [Rule | T]) ->
    error_logger:info_msg("Processing ~s with ~p~n", [Node#stagingnode.mac, Rule]),

    XPath = Rule#stagingrule.xpath,
    Match = try xmerl_xpath:string(XPath, Node#stagingnode.inventory) of
		[] -> false;
		_ -> true
	    catch
		Type:Error ->
		    error_logger:error_msg("~p:~p while processing ~s:~s",
					   [Type, Error,
					    Rule#stagingrule.name, XPath]),
		    false
	    end,
    case Match of
	true ->
	    process_rule(Node, profile, Rule, T);
	false ->
	    error_logger:info_msg("No match~n", []),
	    process_rules(Node, T)
    end;
process_rules(Node, []) -> 
    F = fun() ->
		mnesia:write(stagingnodes, Node, write)
	end,
    {atomic, ok} = mnesia:transaction(F),

    gen_event:notify(host_events,
		     {system, stagingnode, added, Node#stagingnode.mac}),
    ok.

start_discovery(Mac) ->
    Pid = spawn_link(fun() ->
			     Inventory = baracus_driver:get_inventory(Mac),
			     Node = #stagingnode{mac=Mac,
						 inventory=insert_globalsetting("mac", Mac, Inventory)},
			     StagingRules = enum_rules_i(),
			     ok = process_rules(Node, StagingRules),
			     erlang:exit(normal)
		     end),
    {ok, Pid}.

enum_rules_i() ->
    F = fun() ->
		Handle = qlc:q([X || X <- mnesia:table(stagingrules)]),
		SortFun = fun(A, B) ->
				  A#stagingrule.priority < B#stagingrule.priority
			  end,
		qlc:e((qlc:sort(Handle, [{order, SortFun}])))
	end,
    {atomic, Rules} = mnesia:transaction(F),
    Rules.

deploy(Principal, Node) ->
    Mac = Node#stagingnode.mac,
    PowerConfig = #powerconfig{mac=Mac,
			       type=Node#stagingnode.type,
			       host=Node#stagingnode.host,
			       username=Node#stagingnode.username,
			       password=Node#stagingnode.password,
			       bmcaddr=Node#stagingnode.bmcaddr
			      },
    case host_fsm:configure_power(Mac, PowerConfig) of
	ok ->
	    gen_event:notify(host_events,
			     {Principal, stagingnode, deployed, Mac}),
	    ok;
	Else -> Else
    end.

reject(Principal, Node) ->
    Mac = Node#stagingnode.mac,
    gen_event:notify(host_events, {Principal, stagingnode, rejected, Mac}),
    ok.

id_to_filename(Id) ->
    resolvers_path() ++ Id.

insert_globalsetting(Name, Value, Inventory) ->
    insert_setting("/node[@class=\"system\"]/configuration", Name, Value, Inventory).

insert_powersetting(Name, Value, Inventory) ->
    insert_setting("//node[@id=\"powercontroller\"]/configuration", Name, Value, Inventory).

insert_setting(XPath, Name, Value, Inventory) ->
    E = #xmlElement{name=setting,
		    attributes=[
				#xmlAttribute{name=id, value=Name},
				#xmlAttribute{name=value, value=Value}
			       ]
		   },
    xmerl_dom:insert(E, XPath, Inventory, [{position, insert_under}]).

cmd_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
	    cmd_receive(Port, string:concat(Acc, Data));
        {Port, {exit_status, Status}} ->
	    {ok, Status, Acc}
    end.

stream_chunk(Data, Port) ->
    ChunkSize=128,
    true = port_command(Port, string:substr(Data, 1, ChunkSize)),
    stream_chunk(string:substr(Data, ChunkSize+1), Port);
stream_chunk([], Port) ->
    ok.

validate_ref(_, undefined) ->
    ok;
validate_ref(Db, Key) ->
    case mnesia:read(Db, Key, read) of
	[_] -> ok;
	_ -> throw({badarg, Key})
    end.

select_attribute(XPath, Doc) ->
    [#xmlAttribute{value=Value}] = xmerl_xs:select(XPath, Doc),
    Value.

    

