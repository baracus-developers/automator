-module(webapp_server).
-behavior(gen_server).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include_lib("yaws_security/include/yaws_security.hrl").

-export([
    start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    yaws_security_openid:register_provider(),

    ok = yaws_security_openid:create_filter(myopenid,
					    [{login_redirect, "/login"}]),

    ok = yaws_security:register_filterchain(
	   mychain,
	   [ {function, fun(Arg, Ctx) -> forbidden_filter(Arg, Ctx) end},
	     {chain, http_session}, 
	     {chain, myopenid}
	   ],
	   []),

    ok = yaws_security:register_realm("/", mychain,
				      {function,
				       fun(Arg, Ctx) -> handler(Arg, Ctx) end},
				      []),

    ok = yaws_security:register_userdetails(fun(Token) -> userdetails(Token) end),

    PrivDir = code:priv_dir(cloudbuilder),

    GC = yaws_config:make_default_gconf(false, "webapp"),
    SC = #sconf{
      port = 8001,
      servername = "localhost",
      listen = {0, 0, 0, 0},
      docroot = PrivDir ++ "/webui/static",
      appmods = [{"/", yaws_security_filterchain}]
    },

    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error -> {stop, Error}
    end,

    {ok, null}.

forbidden_filter(Arg, Ctx) ->
    try yaws_security_filterchain:next(Arg, Ctx)
    catch
	throw:forbidden -> {redirect_local, "/forbidden"};
	throw:{login_failed, Error} ->
	    Msg = case Error of
		      {connection_failed, _} ->
			  "IDP connection failed";
		      _ ->
			  "Unspecified login failure"
		  end,
	    Url = wf:f("/login?msg=~s", [wf:url_encode(Msg)]),
	    {redirect_local, Url}
    end.

% this function is invoked by the yaws-security framework after a user has successfully
% authenticated with their respective identity provider.  We want to now ensure that
% the user is valid to our application 
userdetails(Token) ->
    Principal = Token#token.principal,
    {ok, {openid, Admin}} = config:get_admin(),

    if
	Admin =:= Principal ->
	    OrigGA = Token#token.granted_authorities,
	    NewGA = sets:union(OrigGA, sets:from_list([role_user, role_admin])),
	    {ok, Token#token{principal="Admin", granted_authorities = NewGA}};
	true ->
	    {ok, Token#token{granted_authorities=sets:new()}}
    end.

handler(Arg, Ctx) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,

    RequestBridge = simple_bridge:make_request(yaws_request_bridge, Arg),
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Arg),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    % install nitrogen handlers for yaws-security
    nitrogen:handler(ys_security_handler, Ctx),
    nitrogen:handler(ys_role_handler, Ctx),
    nitrogen:handler(ys_identity_handler, Ctx),

    HeadPath = case string:tokens(Path, "/") of
	[] -> "root";
	[H | T] -> H
    end,

    % handle default role checking - list unprotected pages explicitly,
    % assume all others require at least role_user
    case HeadPath of
	"login" -> ok;
	"forbidden" -> ok;
	"nitrogen" -> ok;
	"css" -> ok;
	"images" -> ok;
	_ -> yaws_security_context:caller_in_role(Ctx, role_user)
    end,

    nitrogen:run().

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
