-module(bahost_parser).
-include_lib("bahost_record.hrl").
-export([process/1]).

divider() -> "------------------------------------------------------------".
header() ->  "mac                pxecurr    pxenext    state     active   ".

process(RawData) ->
    try
	RawLines = string:tokens(RawData, "\n"),
	StrippedLines = parseheader(RawLines),
	lists:map(fun(X) -> parseline(X) end, StrippedLines)
    catch
	error:Reason ->
		erlang:error({Reason, RawData, erlang:get_stacktrace()})
    end.

% FSM to scan the stream for the presence of the expected header
parseheader([H | T], State) ->
    Header = header(),
    Divider = divider(),
    case State of
	firstdivider ->
	    if
		H =:= Divider ->
		    parseheader(T, header);
		true ->
		    parseheader(T, firstdivider)
	    end;
	header ->
	    if
		H =:= Header ->
		    parseheader(T, seconddivider);
		true ->
		    parseheader(T, firstdivider)
	    end;
	seconddivider ->
	    if
		H =:= Divider ->
		    T;
		true ->
		    parseheader(T, firstdivider)
	    end
    end;
parseheader([], _)->
    erlang:error("Failed to find expected header").

parseheader(Tokens) ->
    parseheader(Tokens, firstdivider).

validmac() -> "([0-9A-F][0-9A-F]:){5}[0-9A-F][0-9A-F]".

parseline(Line) ->
    [Mac, PxeCurr, PxeNext, State, Active] = string:tokens(Line, " "),
    case re:run(Mac, validmac(), [{capture, first, list}]) of
	{match, [CapturedMac]} ->
	    if
		Mac =:= CapturedMac ->
		    {Mac, #bahost{mac = Mac,
				  pxecurr = list_to_atom(PxeCurr),
				  pxenext = list_to_atom(PxeNext),
				  state = list_to_atom(State),
				  active = list_to_atom(Active)}};
		true ->
		    erlang:error("Failed to parse data stream")
	    end;
	_ ->
	    erlang:error("Failed to parse data stream")
    end.
