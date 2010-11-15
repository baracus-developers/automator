-module(config).
-behavior(gen_server).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

-record(state, {domain, admin}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {Xml, _} = xmerl_scan:file("/etc/cloudbuilder.conf"),
    [#xmlText{value=Domain}] = xmerl_xpath:string("//domain/text()", Xml),
    [#xmlText{value=Cookie}] = xmerl_xpath:string("//cookie/text()", Xml),
    [#xmlAttribute{value=AdminType}] = xmerl_xpath:string("//admin/@type", Xml),

    Admin = case AdminType of
		"openid" ->
		    [#xmlText{value=URI}] =
			xmerl_xpath:string("//admin/text()", Xml),
		    {openid, URI};
		Type ->
		    io:format("Ignoring unknown admin-type: ~p~n", [Type])
	    end,
    
    erlang:set_cookie(node(), list_to_atom(Cookie)),

    {ok, #state{domain = Domain, admin = Admin}}.

get_domain() ->
    gen_server:call({global, ?MODULE}, get_domain).

get_admin() ->
    gen_server:call({global, ?MODULE}, get_admin).

handle_call(get_domain, _From, State) ->
    {reply, {ok, State#state.domain}, State};

handle_call(get_admin, _From, State) ->
    {reply, {ok, State#state.admin}, State}.

terminate(Reason, State) ->
    ok.
