-module(webapp_server).
-behavior(gen_server).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

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
	   [ {chain, http_session}, 
	     {chain, myopenid}
	   ],
	   []),

    ok = yaws_security:register_realm("/", mychain,
				      {function,
				       fun(Arg, Ctx) -> handler(Arg, Ctx) end},
				      []),

    GC = yaws_config:make_default_gconf(false, "webapp"),
    SC = #sconf{
      port = 8001,
      servername = "localhost",
      listen = {0, 0, 0, 0},
      docroot = "./monitor/site/static",
      appmods = [{"/", yaws_security_filterchain}]
    },

    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error -> {stop, Error}
    end,

    {ok, null}.
 
handler(Arg, Ctx) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,

    RequestBridge = simple_bridge:make_request(yaws_request_bridge, Arg),
    ResponseBridge = simple_bridge:make_response(yaws_response_bridge, Arg),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    % install nitrogen handlers for yaws-security
%    nitrogen:handler(ys_security_handler, Ctx),
%    nitrogen:handler(ys_role_handler, Ctx),
%    nitrogen:handler(ys_identity_handler, Ctx),

    % handle default role checking - list unprotected pages explicitly,
    % assume all others require at least role_user
%    case Path of
%	"/login" -> ok;
%	_ -> yaws_security_context:caller_in_role(Ctx, role_user)
%    end,

    nitrogen:run().

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
