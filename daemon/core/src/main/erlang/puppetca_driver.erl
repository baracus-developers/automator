-module(puppetca_driver).
-include_lib("certentry.hrl").
-export([start_link/0, refresh/0, clear/1, sign/1, find/1]).

tablename() -> puppetca.

start_link() ->
    delta_keyvalue:start_link(tablename()).
 
refresh() ->
    RawData = os:cmd("puppetca -l --all"),
    Data = puppetca_parser:process(RawData),
    delta_keyvalue:analyze(tablename(), Data).

cmd(Option, Host) ->
    Cmd = io_lib:format("puppetca ~s ~s", [Option, Host]),
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


