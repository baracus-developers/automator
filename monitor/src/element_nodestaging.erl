-module(element_nodestaging).
-export([reflect/0, render_element/1, event/1, inplace_textbox_event/2]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").
-include("staging.hrl").

reflect() -> record_info(fields, nodestaging).

coalesce(Value) ->
    wf:coalesce([Value, "-"]).

-record(node, {mac, selected, data}).

render_controlbar() ->
    #panel{class="staging-controlbar",
	   body=[
		 #link{text="Apply Profile", delegate=?MODULE, postback=profile_apply},
		 " ",
		 #link{text="Apply Resolver", delegate=?MODULE, postback=resolver_apply},
		 " ",
		 #link{text="Deploy", delegate=?MODULE, postback=deploy_selected},
		 " ",
		 #link{text="Reject", delegate=?MODULE, postback=reject_selected}
		]
	  }.

update_db([Record | T], Db) ->
    Node = case dict:find(Record#stagingnode.mac, Db) of
	       {ok, Value} -> Value;
	       error -> #node{mac=Record#stagingnode.mac, selected=false}
	   end,
    update_db(T, dict:store(Node#node.mac, Node#node{data=Record}, Db));
update_db([], Db) ->
    Db.

get_stagingnodes() ->
    Db = wf:state_default(stagingnodes, dict:new()),
    {ok, Records} = staging_server:enum_nodes(),

    UpdatedDb = update_db(Records, Db),

    wf:state(stagingnodes, UpdatedDb),
    UpdatedDb.

subst(X) ->
    case X of
       $: -> $-;
       Else -> Else
    end.

flatten_node(Node) ->
    StagingNode = Node#node.data,
    
    Id = [subst(X) || X <- Node#node.mac],
    SelectedId = "check-" ++ Id,
    TypeId = "type-" ++ Id,

    [_, Zone] = string:tokens(atom_to_list(node()), "@"),

    [
     SelectedId,
     Node#node.selected,
     {node_toggle, SelectedId, Node#node.mac},
     Zone,
     Node#node.mac,
     TypeId,
     coalesce(StagingNode#stagingnode.type),
     {type, TypeId, Node#node.mac},
     coalesce(StagingNode#stagingnode.host),
     {host, Node#node.mac},
     coalesce(StagingNode#stagingnode.bmcaddr),
     {bmcaddr, Node#node.mac},
     coalesce(StagingNode#stagingnode.username),
     {username, Node#node.mac},
     coalesce(StagingNode#stagingnode.password),
     {password, Node#node.mac},
     [
      #link{text="deploy", delegate=?MODULE, postback={node_deploy, Node#node.mac}},
      " ",
      #link{text="reject", delegate=?MODULE, postback={node_reject, Node#node.mac}}
     ]
    ].

render_stagingnodes() ->

    Nodes = [flatten_node(Node) || {_, Node} <- dict:to_list(get_stagingnodes())],

    #cbtable{class="nodes",
	     data=Nodes,
	     map= [
		   selected@id,
		   selected@checked,
		   selected@postback,
		   zone@text,
		   mac@text,
		   type@id,
		   type@value,
		   type@postback,
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
						  options=lists:flatten([
									 #option{text="undefined",
										 value="-"
										}
									],
									[
									 #option{text=Type, value=Type} ||
									    Type <- baracus_driver:get_bmctypes()
									]
								       ),
						  delegate=?MODULE
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
    comet_event_relay:add_handler(host_events, "stagingnodes",
				  fun(Event) -> event(Event) end),   

    Panel = #panel{ body=[
			  #h1{},
			  render_controlbar(),
			  #panel{id="staging-nodes",
				 body=render_stagingnodes()
				}
			 ]
		  },
    element_panel:render_element(Panel).

update_selections(Value) ->
    F = fun(_, Node) ->
		Node#node{selected=Value}
	end,
    Db = wf:state(stagingnodes),
    wf:state(stagingnodes, dict:map(F, Db)),
    wf:update("staging-nodes", render_stagingnodes()),
    ok.

find_selected() ->
    F = fun(_, Node) ->
		Node#node.selected
	end,
    Db = dict:filter(F, wf:state(stagingnodes)),
    [Mac || {Mac, _} <- dict:to_list(Db)].

deploy_node(Mac) ->
    case staging_server:deploy_node(Mac) of
	ok -> "ok";
	{error, Reason} -> "failed: " ++ Reason;
	Else -> "failed"
    end.

render_deploystatus(Macs) ->

    Results = [{Mac, deploy_node(Mac)} || Mac <- Macs],

    Panel = #lightbox{ id="deploy-status",
		       body=#panel{class="general-lightbox", 
				   body=[
					 #h2{ text="Deploy status" },
					 #list{ body=[#listitem{text=wf:f("~s (~s)", [Mac, Status])} ||
							 {Mac, Status} <- Results]
					      },
					 #button{text="Ok", postback=close_deploystatus, delegate=?MODULE}
					 ]
				  }
		     },
    wf:insert_top("staging-nodes", Panel).

event({system, stagingnode, _Operation, _Profile}) ->
    wf:update("staging-nodes", render_stagingnodes()),
    wf:flush();
event(nodes_select_all) ->
    update_selections(true),
    ok;
event(nodes_select_none) ->
    update_selections(false),
    ok;
event({node_toggle, Id, Mac}) ->
    Value = wf:q(Id),
    Db = wf:state(stagingnodes),
    case dict:find(Mac, Db) of
	{ok, Node} ->
	    NewDb = dict:update(Mac, Node#node{selected=Value}, Db),
	    wf:state(stagingnodes, NewDb)
    end;
event({node_deploy, Mac}) ->
    render_deploystatus([Mac]);
event(deploy_selected) ->
    Macs = find_selected(),
    render_deploystatus(Macs);
event({node_reject, Mac}) ->
    staging_server:reject_node(Mac);
event(reject_selected) ->
    Macs = find_selected(),
    [staging_server:reject_node(Mac) || Mac <- Macs];
event(close_deploystatus) ->
    wf:remove("deploy-status");
event({type, Id, Mac}) ->
    Value = wf:q(Id),
    staging_server:set_param(Mac, type, Value);
event(Event) ->
    ok.

inplace_textbox_event({Type, Mac}, Value) ->
    staging_server:set_param(Mac, Type, Value),
    Value.


