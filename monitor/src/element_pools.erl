-module(element_pools).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, pools).

render_element(R) ->
    Panel = #panel{ id=poolsPanel, body=#h1 { text="Pools!" }},
    element_panel:render_element(Panel).
