-module(api_handler).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-export([out/1, handle_request/4]).

out(Arg) ->
    Req = Arg#arg.req,
    Headers = Arg#arg.headers,
    ReqPath = string:tokens(get_path(Arg), "/"),
    Accepts = string:tokens(Headers#headers.accept, ","),
    process(Req#http_request.method, Accepts, ReqPath, Arg).

process(Cmd, [Accept | T], Request, Arg) ->
    try handle_request(Cmd, Accept, Request, Arg)
    catch
	throw:nomatch -> process(Cmd, T, Request, Arg)
    end;					 
process(Cmd, [], Request, Arg) ->
    error_logger:info_msg("Ignoring ~p ~p (~p)~n",
			  [Cmd, Request, Arg]),
    make_response(404, "<p>Page not found</p>").

get_path(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,
    Path.

hostify(Host) ->
    "<host>" ++ Host ++ "</host>".

handle_request('GET', "application/xml", ["hosts"], _Arg) ->
    {ok, Hosts} = hosts_server:enum(),
    Data = "<hosts>" ++ [hostify(Host) || Host <- Hosts] ++ "</hosts>",
    make_response(200, Data);

handle_request(Cmd, Accept, Request, Arg) -> % catchall
    throw(nomatch).

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) ->
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].
