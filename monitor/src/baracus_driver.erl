-module(baracus_driver).
-include_lib("bahost_record.hrl").
-include("power.hrl").
-compile(export_all).

tablename() -> bahost.

run_once(Nodes) ->
    io:format("Initializing bahost table~n"),
    {atomic, ok} = delta_keyvalue:run_once(tablename(), Nodes).

start_link() ->
    delta_keyvalue:start_link(tablename()).
 
status(Mac) ->
    void.

paramdecode([{_, undefined} | T], Acc) ->
    paramdecode(T, Acc);
paramdecode([{mac, _Val} | T], Acc) ->
    paramdecode(T, Acc);
paramdecode([{host, Val} | T], Acc) ->
    paramdecode(T, Acc ++ " --host " ++ Val);
paramdecode([{bmcaddr, Val} | T], Acc) ->
    paramdecode(T, Acc ++ " --bmcaddr " ++ Val);
paramdecode([{type, Val} | T], Acc) ->
    paramdecode(T, Acc ++ " --ctype " ++ Val);
paramdecode([{username, Val} | T], Acc) ->
    paramdecode(T, Acc ++ " --login " ++ Val);
paramdecode([{password, Val} | T], Acc) ->
    paramdecode(T, Acc ++ " --passwd " ++ Val);
paramdecode([], Acc) ->
    Acc.

configure_power(Mac, Config) ->
    [_ | RawParams] = tuple_to_list(Config),
    Fields = record_info(fields, powernode),
    Params = lists:zip(Fields, RawParams),
    
    Cmd = paramdecode(Params, "bapower add --mac " ++ Mac),

    io:format("Cmd: ~s~n", [Cmd]),

    util:os_cmd(Cmd),

    gen_event:notify(host_events, {baracus, power_configured, Mac}),
    ok.

provision(Mac, Hostname, default) ->
    gen_event:notify(host_events, {policy, provision, Mac, Hostname}),
    util:os_cmd_format("bado build --mac ~s --profile sumatra --ip dhcp --hostname ~s --module puppet", [Mac, Hostname]).

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
    Inventory = util:os_cmd_format("bahost detail inventory --mac ~s", [Mac]),
    {InvetoryXml, _} = xmerl_scan:string(Inventory),

    InvetoryXml.

refresh() ->
    RawData = util:os_cmd("bahost list states"),
    Data = bahost_parser:process(RawData),
    delta_keyvalue:analyze(tablename(), Data).

get_state(Mac) ->
    case delta_keyvalue:find(tablename(), Mac) of
	notfound ->
	    notfound;
	{ok, Record} ->
	    {ok, Record#bahost.state}
    end.

get_bmctypes() ->
    ['ipmi',  'virsh',  'bladecenter',  'ilo', 'drac', 'vmware', 'apc', 'wti',  'egenera', 'mainframe'].
