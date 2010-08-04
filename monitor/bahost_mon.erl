-module(bahost_mon).
-export([start_link/0]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

poller() ->
    RawData = os:cmd("bahost list states"),
    Data = bahost_parser:process(RawData),
    bahost_analyzer:analyze(Data),
    timer:sleep(5000),
    poller().


