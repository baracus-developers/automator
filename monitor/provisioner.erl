-module(provisioner).
-compile(export_all).

init(_Args) ->
    {ok, []}.

handle_event({system, discovery, {Mac, _Inventory}}, State) ->
    host_state:configure_power(Mac, ""),
    host_state:provision(Mac, default),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.


