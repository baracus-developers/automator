-module(baracus_driver).
-compile(export_all).

status(Mac) ->
	void.

provision(Mac, Hostname, _Personality) ->
    Cmd = io_lib:format("bado build --mac ~s --profile sumatra --ip dhcp --hostname sumatra-~s --module=puppet", [Mac, Hostname]),
    gen_event:notify(host_events, {policy, provision, Mac, Hostname}),
    os:cmd(Cmd).

