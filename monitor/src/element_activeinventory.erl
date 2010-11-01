-module(element_activeinventory).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include_lib("hostinfo.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, activeinventory).

flatten(Selected, Mac, Pool, Category) ->
    {ok, Host} = hosts_server:get_hostinfo(Mac),

    SelectedId = wf:temp_id(),

    [
     SelectedId,
     sets:is_element(Mac, Selected),
     {node_toggle, SelectedId, Mac},
     Host#hostinfo.zone,
     Mac,
     wf:f("~s/~p", [Pool, Category]),
     "",
     Host#hostinfo.state
    ].

get_members(Pool) ->
    {ok, Members} = pools_server:enum_members(Pool),
    [{Mac, Pool, Category} || {Mac, Category} <- Members].

get_pools() ->
    {ok, Pools} = pools_server:enum(),
    Pools.

render_table() ->
    Members = lists:flatten([get_members(Pool) || Pool <- get_pools()]),
    
    Macs = [Mac || {Mac, _, _} <- Members],
    Selected = sets:intersection([wf:state_default(selectednodes, sets:new()),
				  sets:from_list(Macs)]),
    wf:state(selectednodes, Selected),

    Rows = [flatten(Selected, Mac, Pool, Category) 
	    || {Mac, Pool, Category} <- Members],
    
    #cbtable{class="nodes",
	     data=Rows,
	     map= [
		   selected@id,
		   selected@checked,
		   selected@postback,
		   zone@text,
		   mac@text,
		   pool@text,
		   service@text,
		   status@text
		  ],
             header=[
		     #tableheader{body=[
					#link{text="all",
					      delegate=?MODULE,
					      postback=nodes_select_all},
					" ",
					#link{text="none",
					      delegate=?MODULE,
					      postback=nodes_select_none}
				       ]
				 },
		     #tableheader { text="Zone" },
		     #tableheader { text="MAC" },
		     #tableheader { text="Pool" },
		     #tableheader { text="Service" },
		     #tableheader { text="Status" }
		    ],
	     rowspec=[
		      #tablecell { body=#checkbox{id=selected, delegate=?MODULE} },
		      #tablecell { id=zone },
		      #tablecell { id=mac },
		      #tablecell { id=pool },
		      #tablecell { id=service },
		      #tablecell { id=status }
		     ]
	    }.

handle_event(Id, Event) ->
    wf:update(Id, render_table()),
    wf:flush().

render_nodes() ->
    Id = wf:temp_id(),
    comet_event_relay:add_handler(pool_events, Id ++ "-pools",
				  fun(Event) -> handle_event(Id, Event) end),   
    comet_event_relay:add_handler(host_events, Id ++ "-hosts",
				  fun(Event) -> handle_event(Id, Event) end),   
    comet_event_relay:add_handler(machine_events, Id ++ "-machines",
				  fun(Event) -> handle_event(Id, Event) end),   
    [
     #h1{ text="Nodes"},
     #panel{ id=Id, body = render_table()}
    ].

render_element(R) ->
    Panel = #panel{ body=render_nodes() },
    element_panel:render_element(Panel).

event(_) ->
    ok.
