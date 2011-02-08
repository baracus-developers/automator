-module(ui_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->

    %{ok, Port} = application:get_env(?MODULE, port),
    Port = 8000,

    ui_sup:start_link(Port).

stop(_State) -> ok.


