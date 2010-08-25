-module(config).
-behavior(gen_server).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

-record(state, {domain}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {Xml, _} = xmerl_scan:file("/etc/cloudbuilder.conf"),
    [#xmlText{value=Domain}] = xmerl_xpath:string("//domain/text()", Xml),
    [#xmlText{value=Cookie}] = xmerl_xpath:string("//cookie/text()", Xml),

    erlang:set_cookie(node(), list_to_atom(Cookie)),

    {ok, #state{domain = Domain}}.

get_domain() ->
    gen_server:call({global, ?MODULE}, get_domain).

handle_call(get_domain, _From, State) ->
    {reply, {ok, State#state.domain}, State}.

terminate(Reason, State) ->
    ok.
