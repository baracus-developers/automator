-module(util).
-export([os_cmd_format/2, os_cmd/2, os_cmd/1]).

os_cmd_format(CmdFormat, Params) ->
    EncodedCmd = io_lib:format(CmdFormat, Params),    
    Cmd = unicode:characters_to_list(EncodedCmd),
    os_cmd(Cmd).

os_cmd(Cmd) ->
    os_cmd(Cmd, 0).

os_cmd(Cmd, ExpectedStatus) ->
    Port = erlang:open_port({spawn, Cmd}, [use_stdio, exit_status]),
    case cmd_receive(Port, "") of
	{ok, ExpectedStatus, Data} ->
	    Data;
	{ok, UnexpectedStatus, Data} ->
	    throw({badstatus, {{status, UnexpectedStatus}, {cmd, Cmd}}})
    end.	

cmd_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
	    cmd_receive(Port, string:concat(Acc, Data));
        {Port, {exit_status, Status}} ->
	    {ok, Status, Acc}
    end.


