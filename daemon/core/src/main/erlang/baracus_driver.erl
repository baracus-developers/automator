-module(baracus_driver).
-include_lib("bahost_record.hrl").
-include("baracus.hrl").
-compile(export_all).

tablename() -> bahost.

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

configure_power(Mac, Config) when is_record(Config, powerconfig) ->
    [_ | RawParams] = tuple_to_list(Config),
    Fields = record_info(fields, powerconfig),
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
    util:os_cmd_format("bapower on --mac ~s", [Mac]),
    gen_event:notify(host_events, {baracus, poweron, Mac}),
    void.

poweroff(Mac) ->
    util:os_cmd_format("bapower off --mac ~s", [Mac]),
    gen_event:notify(host_events, {baracus, poweroff, Mac}),
    void.

powercycle(Mac) ->
    util:os_cmd_format("bapower cycle --mac ~s", [Mac]),
    gen_event:notify(host_events, {baracus, powercycle, Mac}),
    void.

powerstatus(Mac) ->
    on.

get_inventory(Mac) -> 
    Inventory = util:os_cmd_format("bahost detail inventory --mac ~s", [Mac]),
    {InvetoryXml, _} = xmerl_scan:string(Inventory),

    InvetoryXml.

refresh(Zone) ->
    RawData = util:os_cmd("bahost list states"),
    Data = bahost_parser:process(Zone, RawData),
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
