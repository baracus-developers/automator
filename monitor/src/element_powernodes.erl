-module(element_powernodes).
-export([reflect/0, render_element/1, event/1, inplace_textbox_event/2]).

-include_lib("nitrogen/include/wf.inc").
-include("power.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, powernodes).

coalesce(Value) ->
    wf:coalesce([Value, "-"]).

-record(node, {mac, selected, data}).

update_db([Record | T], Db) ->
    Node = case dict:find(Record#powernode.mac, Db) of
	       {ok, Value} -> Value;
	       error -> #node{mac=Record#powernode.mac, selected=false}
	   end,
    update_db(T, dict:store(Node#node.mac, Node#node{data=Record}, Db));
update_db([], Db) ->
    Db.

get_powernodes() ->
    Db = wf:state_default(powernodes, dict:new()),
    {ok, Records} = power_server:enum_nodes(),

    UpdatedDb = update_db(Records, Db),

    wf:state(powernodes, UpdatedDb),
    UpdatedDb.

subst(X) ->
    case X of
       $: -> $-;
       Else -> Else
    end.

flatten_node(Node) ->
    PowerNode = Node#node.data,
    
    Id = [subst(X) || X <- Node#node.mac],
    SelectedId = "check-" ++ Id,

    [_, Zone] = string:tokens(atom_to_list(node()), "@"),

    [
     SelectedId,
     Node#node.selected,
     {node_toggle, SelectedId, Node#node.mac},
     Zone,
     Node#node.mac,
     coalesce(PowerNode#powernode.type),
     coalesce(PowerNode#powernode.host),
     {host, Node#node.mac},
     coalesce(PowerNode#powernode.bmcaddr),
     {bmcaddr, Node#node.mac},
     coalesce(PowerNode#powernode.username),
     {username, Node#node.mac},
     coalesce(PowerNode#powernode.password),
     {password, Node#node.mac},
     [
      #link{text="submit", delegate=?MODULE, postback={node_submit, Node#node.mac}}
     ]
    ].

render_powernodes() ->

    Nodes = [flatten_node(Node) || {_, Node} <- dict:to_list(get_powernodes())],

    #cbtable{class="nodes",
	     data=Nodes,
	     map= [
		   selected@id,
		   selected@checked,
		   selected@postback,
		   zone@text,
		   mac@text,
		   type@value,
		   host@text,
		   host@tag,
		   bmcaddr@text,
		   bmcaddr@tag,
		   username@text,
		   username@tag,
		   password@text,
		   password@tag,
		   actions@body
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
		     #tableheader{text="Zone"},
		     #tableheader{text="MAC"},
		     #tableheader{text="Type"},
		     #tableheader{text="Host"},
		     #tableheader{text="BMC Address"},
		     #tableheader{text="Username"},
		     #tableheader{text="Password"},
		     #tableheader{text="Actions"}
		    ],
	     rowspec=[
		      #tablecell { body=#checkbox{id=selected, delegate=?MODULE}},
		      #tablecell { id=zone },
		      #tablecell { id=mac },
		      #tablecell { body=#dropdown{id=type,
						  options=[
							   #option{text="undefined",
								   value="-"
								  },
							   #option{text="IPMI",
								   value="IPMI"
								  }
							  ]
						 }
				 },
		      #tablecell { body=#inplace_textbox{id=host,
							 delegate=?MODULE
							}
				 },
		      #tablecell { body=#inplace_textbox{id=bmcaddr,
							 delegate=?MODULE
							}
				 },
		      #tablecell { body=#inplace_textbox{id=username,
							 delegate=?MODULE
							}
				 },
		      #tablecell { body=#inplace_textbox{id=password,
							 delegate=?MODULE
							}
				 },
		      #tablecell { id=actions }
		     ]
	    }.

render_element(R) ->
    comet_event_relay:add_handler(host_events, "powernodes",
				  fun(Event) -> event(Event) end),   

    Panel = #panel{ body=#panel{id="power-nodes",
				body=render_powernodes()
			       }
		  },
    element_panel:render_element(Panel).

update_selections(Value) ->
    F = fun(_, Node) ->
		Node#node{selected=Value}
	end,
    Db = wf:state(powernodes),
    wf:state(powernodes, dict:map(F, Db)),
    wf:update("power-nodes", render_powernodes()),
    ok.

event({system, powernode, _Operation, _Profile}) ->
    wf:update("power-nodes", render_powernodes()),
    wf:flush();
event(nodes_select_all) ->
    update_selections(true),
    ok;
event(nodes_select_none) ->
    update_selections(false),
    ok;
event({node_toggle, Id, Mac}) ->
    Value = wf:q(Id),
    Db = wf:state(powernodes),
    case dict:find(Mac, Db) of
	{ok, Node} ->
	    NewDb = dict:update(Mac, Node#node{selected=Value}, Db),
	    wf:state(powernodes, NewDb)
    end;
event({node_submit, Mac}) ->
    power_server:submit_node(Mac);
event(Event) ->
    ok.

inplace_textbox_event({Type, Mac}, Value) ->
    power_server:set_param(Mac, Type, Value),
    Value.


