-module(element_serviceelements).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, serviceelements).

render_element(R) ->
    Panel = #panel{},
    element_panel:render_element(Panel).
