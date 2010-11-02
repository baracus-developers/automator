-module(services).
-behavior(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include("services.hrl").

-export([start_link/0, init/1,
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([create/1, enum/0, delete/1]).
-export([create_element/4, enum_elements/1, delete_element/1]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    ok = util:open_table(services,
			 [
			  {record_name, service},
			  {attributes,
			   record_info(fields, service)},
			  {disc_copies, util:replicas()}
			 ]),
    ok = util:open_table(elements,
			 [
			  {record_name, element},
			  {attributes,
			   record_info(fields, element)},
			  {disc_copies, util:replicas()}
			 ]),
    {ok, #state{}}.

enum() ->
    gen_server:call(?MODULE, enum).

create(Name) ->
    gen_server:call(?MODULE, {create, Name}).

delete(Name) ->
    gen_server:call(?MODULE, {delete, Name}).

create_element(Service, Type, Pools, Elasticity) ->
    gen_server:call(?MODULE, {create_element, Service, Type, Pools, Elasticity}).

enum_elements(Service) ->
    gen_server:call(?MODULE, {enum_elements, Service}).

delete_element(Id) ->
    gen_server:call(?MODULE, {delete_element, Id}).

handle_call(enum, _From, State) ->
    Services = util:atomic_query(qlc:q([Service#service.name ||
					   Service <- mnesia:table(services)])),
    {reply, {ok, Services}, State};

handle_call({enum_elements, Service}, _From, State) ->
    try
	Elements = util:atomic_query(qlc:q([E || E <- mnesia:table(elements),
						 E#element.service == Service])),
	{reply, {ok, Elements}, State}
    catch
	Type:Error ->
	    {reply, {error, {Type, Error}}, State}
    end;

handle_call({create_element, Service, Type, Pools, Elasticity}, _From, State) ->
    F = fun() ->
		[_] = mnesia:read(services, Service, read),
		Id = uuid:to_string(uuid:v4()),
		Element = #element{id=Id,
				   service=Service,
				   type=Type,
				   pools=Pools,
				   elasticity=Elasticity
				  },
		mnesia:write(elements, Element, write)
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->
	    {reply, ok, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call({delete_element, Id}, _From, State) ->
    F = fun() -> mnesia:delete(elements, Id, write) end,
    {atomic, ok} = mnesia:transaction(F),
    {reply, ok, State};

handle_call({create, Name}, _From, State) ->
    F = fun() ->
		case mnesia:read(services, Name, write) of
		    [] ->
			Service = #service{name=Name},
			mnesia:write(services, Service, write);
		    [Record] ->
			exists
		end
	end,
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    {reply, Result, State};
	Error ->
	    {reply, {error, Error}, State}
    end;
	
handle_call({delete, Name}, _From, State) ->
    F = fun() ->
		case mnesia:read(services, Name, write) of
		    [] ->
			notfound;
		    [Record] ->
			mnesia:delete(services, Name, write)
		end
	end,
    case mnesia:transaction(F) of
	{atomic, Result} ->
	    {reply, Result, State};
	Error ->
	    {reply, {error, Error}, State}
    end;

handle_call(Request, _From, State) ->
    {stop, {unexpected_call, Request}, State}.

handle_cast(Request, State) ->
    throw(unexpected).

handle_info(Info, State) ->
    throw(unexpected).

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
