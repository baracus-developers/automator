-module(element_servicespanel).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, servicespanel).

render_body() ->
    [
     #backsplash{class="services", body=#services{delegate=?MODULE,
						  postback=selected}},
     #backsplash{id="service-elements",
		 body=#panel{}}
    ].

render_element(R) ->
    Panel = #panel{ body=render_body()},
    element_panel:render_element(Panel).

event({selected, none}) ->
    wf:update("service-elements", #panel{});
event({selected, Service}) ->
    wf:update("service-elements", #serviceelements{name=Service});

event(_) ->
    ok.

