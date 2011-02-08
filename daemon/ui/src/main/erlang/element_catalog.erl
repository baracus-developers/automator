-module(element_catalog).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, catalog).

render_element(R) ->
    Panel = #panel{},
    element_panel:render_element(Panel).
