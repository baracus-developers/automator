-module(element_servicespanel).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, servicespanel).

panels() ->
    [
     {"Services",   #services{}},
     {"Elements",   #serviceelements{}},
     {"Catalog",    #catalog{}}
    ].

render_element(R) ->
    Panel = #panel{ body=#tabbedbacksplash{panels=panels()}},
    element_panel:render_element(Panel).

