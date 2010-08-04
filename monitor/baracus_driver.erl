-module(baracus_driver).
-compile(export_all).

status(Mac) ->
	void.

provision(Mac, Hostname, default) ->
    Cmd = io_lib:format("bado build --mac ~s --profile sumatra --ip dhcp --hostname sumatra-~s --module=puppet", [Mac, Hostname]),
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

get_inventory(Mac) ->
    "<inventory/>".
