-module(element_inventorypanel).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include_lib("hostinfo.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventorypanel).

get_host(Host) ->
    {ok, HostInfo} = hosts_server:get_hostinfo(Host),
    [Host, atom_to_list(HostInfo#hostinfo.state)].

get_hosts() ->
    {ok, Hosts} = hosts_server:enum(),
    [get_host(Host) || Host <- Hosts].

get_pools() ->
    {ok, Pools} = pools_server:enum(),
    Pools.

render_table() ->
    #cbtable{class="nodes",
	     data=get_hosts(),
	     map= [
		   macLabel@text, 
		   statusLabel@text
		  ],
             header=[
		     #tableheader { text="" },
		     #tableheader { text="MAC" },
		     #tableheader { text="Status" }
		    ],
	     rowspec=[
		      #tablecell { body=#checkbox{} },
		      #tablecell { id=macLabel },
		      #tablecell { id=statusLabel }
		     ]
	    }.

handle_event(Id, Event) ->
    wf:update(Id, render_table()),
    wf:flush().

render_pools() ->
    #backsplash{ body=[
		       #h1{text="Pools"},
		       #dropdown{id=select_pool,
				 value="Default",
				 postback=select_pool,
				 delegate=?MODULE,
				 options=[
					  #option{text=Name, value=Name}
					  || Name <- get_pools()
					 ]
				},
		       #p{},
		       #google_chart {
				  title="\"Default\" Pool Capacity",
				  type=pie3d,
				  width=400, height=250,
				  
				  axes=[
					#chart_axis { position=bottom,
						      labels=["In Use",
							      "Free",
							      "Sick"] }
				       ],
				  data=[
					#chart_data { legend="Data 1", 
						      values=[45, 50, 5] 
						    }
				       ]
				 }
		      ]
	       }.

render_nodes() ->
    Id = wf:temp_id(),
    comet_event_relay:add_handler(host_events, Id ++ "-hosts",
				  fun(Event) -> handle_event(Id, Event) end),   
    comet_event_relay:add_handler(machine_events, Id ++ "-machines",
				  fun(Event) -> handle_event(Id, Event) end),   
    #backsplash{ body=[
		       #h1{ text="Nodes"},
		       #panel{ id=Id, body = render_table()}
		      ]
	       }.

render_element(R) ->
    Panel = #panel{ body=[
			 render_pools(),
			 render_nodes()
			 ]
		  },
    element_panel:render_element(Panel).

event(select_pool) ->
    Pool = wf:q(select_pool),
    io:format("Selected: ~p~n", [Pool]).
