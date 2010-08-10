-module(puppetca_driver).
-export([clear/1, sign/1]).

cmd(Option, Host) ->
    Domain = ".laurelwood.net", %FIXME
    Cmd = io_lib:format("puppetca ~s ~s", [Option,
					   string:concat(Host, Domain)]),
    os:cmd(Cmd).    

clear(Host) ->
    gen_event:notify(machine_events, {puppetca, clearing, Host}),
    cmd("-c", Host),

    ok.

sign(Host) ->
    gen_event:notify(machine_events, {puppetca, signing, Host}),
    cmd("-s", Host),

    ok.
    
