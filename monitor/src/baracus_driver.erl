-module(baracus_driver).
-include_lib("bahost_record.hrl").
-compile(export_all).

tablename() -> bahost.

run_once(Nodes) ->
    io:format("Initializing bahost table~n"),
    {atomic, ok} = delta_keyvalue:run_once(tablename(), Nodes).

start_link() ->
    delta_keyvalue:start_link(tablename()).
 
status(Mac) ->
    void.

configure_power(Mac, Config) ->
    gen_event:notify(host_events, {baracus, power_configured, Mac}),
    void.

provision(Mac, Hostname, default) ->
    Cmd = io_lib:format("bado build --mac ~s --profile sumatra --ip dhcp --hostname ~s --module=puppet", [Mac, Hostname]),
    gen_event:notify(host_events, {policy, provision, Mac, Hostname}),
    os:cmd(Cmd).

poweron(Mac) ->
    gen_event:notify(host_events, {baracus, poweron, Mac}),
    void.

poweroff(Mac) ->
    gen_event:notify(host_events, {baracus, poweroff, Mac}),
    void.

powercycle(Mac) ->
    gen_event:notify(host_events, {baracus, powercycle, Mac}),
    void.

powerstatus(Mac) ->
    on.

get_inventory(Mac) ->
    Cmd = io_lib:format("bahost detail inventory --mac ~s", [Mac]),
    os:cmd(Cmd).

refresh() ->
    RawData = os:cmd("bahost list states"),
    Data = bahost_parser:process(RawData),
    delta_keyvalue:analyze(tablename(), Data).

get_state(Mac) ->
    case delta_keyvalue:find(tablename(), Mac) of
	notfound ->
	    notfound;
	{ok, Record} ->
	    {ok, Record#bahost.state}
    end.
