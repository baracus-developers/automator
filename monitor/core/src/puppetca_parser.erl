-module(puppetca_parser).
-include_lib("certentry.hrl").
-compile(export_all).

process(RawData) ->
    try
	RawLines = string:tokens(RawData, "\n"),
	lists:map(fun(X) -> parseline(X) end, RawLines)
    catch
	error:Reason ->
		erlang:error({Reason, RawData, erlang:get_stacktrace()})
    end.

parsetokens(Line) ->
    Tokens = string:tokens(Line, " "),
    case erlang:length(Tokens) of
	2 ->
	    [Host, Fingerprint] = Tokens,
	    #certentry{type = request, host = Host, fingerprint = Fingerprint};
	3 ->
	    [Type, Host, Fingerprint] = Tokens,
	    case Type of
		"+" ->
		    #certentry{type = valid, host = Host,
			       fingerprint = Fingerprint};
		"-" ->
		    #certentry{type = revoked, host = Host,
			       fingerprint = Fingerprint}
	    end
    end.

validfinger() -> "\\(([0-9A-F]{2}:){15}[0-9A-F]{2}\\)".

parseline(Line) ->
    CertEntry = parsetokens(Line),
    case re:run(CertEntry#certentry.fingerprint, validfinger(),
		[{capture, first, list}]) of
	{match, [CapturedFingerprint]} ->
	    if
		CertEntry#certentry.fingerprint =:= CapturedFingerprint ->
		    {CertEntry#certentry.host, CertEntry};
		true ->
		    erlang:error({"Partial valid fingerprint",
				  {item, CertEntry},
				  {captured, CapturedFingerprint}})
	    end;
	Reason ->
	    erlang:error({"Invalid fingerprint",
			  {item, CertEntry}, {reason, Reason}})
    end.
