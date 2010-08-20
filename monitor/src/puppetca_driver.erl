-module(puppetca_driver).
-include_lib("certentry.hrl").
-export([run_once/1, start_link/0, refresh/0, clear/1, sign/1, find/1]).

tablename() -> puppetca.

run_once(Nodes) ->
    io:format("Initializing puppetca table~n"),
    {atomic, ok} = delta_keyvalue:run_once(tablename(), Nodes).

start_link() ->
    delta_keyvalue:start_link(tablename()).
 
refresh() ->
    RawData = os:cmd("puppetca -l --all"),
    Data = puppetca_parser:process(RawData),
    delta_keyvalue:analyze(tablename(), Data).

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

find(Host) ->
    delta_keyvalue:find(tablename(), Host).


