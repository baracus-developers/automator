-module(element_users).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, users).

render_element(R) ->
    Panel = #panel{ id=usersPanel, body=#h1 { text="Users!" }},
    element_panel:render_element(Panel).

