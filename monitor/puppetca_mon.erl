-module(puppetca_mon).
-export([start_link/0]).

start_link() ->
    Pid = proc_lib:spawn_link(fun() -> poller() end),
    {ok, Pid}.

poller() ->
    RawData = os:cmd("puppetca -l --all"),
    Data = puppetca_parser:process(RawData),
    puppetca_analyzer:analyze(Data),
    timer:sleep(5000),
    poller().


