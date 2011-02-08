-module(element_inventorypanel).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventorypanel).

panels() ->
    [
     {"Deployed",   #activeinventory{}},
     {"Staging",    #nodestaging{}},
     {"Automation", #inventoryautomation{}},
     {"Profiles",   #inventoryprofiles{}},
     {"Resolvers",  #inventoryresolvers{}}
    ].

render_element(R) ->
    Panel = #panel{ body=#tabbedbacksplash{panels=panels()}},
    element_panel:render_element(Panel).

