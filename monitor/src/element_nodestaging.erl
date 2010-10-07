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
		 #link{text="Edit", delegate=?MODULE, postback=edit_selected},
		 " ",
		 #link{text="Resolve", delegate=?MODULE, postback=resolve_selected},
		 " ",
		 #link{text="Deploy", delegate=?MODULE, postback=deploy_selected}
% NOTYET						
%		 " ",
%		 #link{text="Reject", delegate=?MODULE, postback=reject_selected}
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

    ValidSet = sets:from_list([Record#stagingnode.mac || Record <- Records]),
    F = fun(Key, _) ->
		sets:is_element(Key, ValidSet)
	end,

    UpdatedDb = update_db(Records, dict:filter(F, Db)),

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

    [_, Zone] = string:tokens(atom_to_list(node()), "@"),

    [
     SelectedId,
     Node#node.selected,
     {node_toggle, SelectedId, Node#node.mac},
     Zone,
     Node#node.mac,
     coalesce(StagingNode#stagingnode.pool),
     coalesce(StagingNode#stagingnode.type),
     coalesce(StagingNode#stagingnode.host),
     coalesce(StagingNode#stagingnode.bmcaddr),
     coalesce(StagingNode#stagingnode.username),
     coalesce(StagingNode#stagingnode.password),
     [
      #link{text="edit", delegate=?MODULE, postback={node_edit, Node#node.mac}},
      " ",
      #link{text="deploy", delegate=?MODULE, postback={node_deploy, Node#node.mac}}
% NOTYET
%      " ",
%      #link{text="reject", delegate=?MODULE, postback={node_reject, Node#node.mac}}
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
		   pool@text,
		   type@text,
		   host@text,
		   bmcaddr@text,
		   username@text,
		   password@text,
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
		     #tableheader{text="Pool"},
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
		      #tablecell { id=pool },
		      #tablecell { id=type},
		      #tablecell { id=host},
		      #tablecell { id=bmcaddr},
		      #tablecell { id=username},
		      #tablecell { id=password},
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

render_edititem(Id, Label) ->
    #panel{ class="edititem",
	    body=[
		  #label{class="edititem-label", text=wf:f("~s: ", [Label])},
		  #inplace_textbox{id=Id,
				   class="edititem-input",
				   delegate=?MODULE,
				   text="undefined"}
		 ]
	  }.

render_edit(Macs) ->

    BmcTypes = [undefined | baracus_driver:get_bmctypes()],

    Table = #cbtable{class="affected-nodes",
		     data=[[Mac] || Mac <- Macs],
		     map= [
			   mac@text
			  ],
		     header=[
			     #tableheader{text=wf:f("Affected Nodes (~p)",
						    [length(Macs)])}
			    ],
		     rowspec=[
			      #tablecell { id=mac }
			     ]
		    },

    Panel = #dialog{ id="edit-nodes",
		     title="Nodes: Group Edit",
		     body=[
			   Table,
			   #panel{class="edit-form",
				  body=[
					render_edititem(pool, "Pool"), 
					render_edititem(host, "Host"), 
					#panel{class="edititem",
					       body=[
						     #label{class="edititem-label", text="Type:"},
						     #dropdown{id=type,
							       class="edititem-input",
							       options=[#option{text=Type, value=Type} ||
									   Type <- BmcTypes
								       ]
							      }
						    ]
					      },
					render_edititem(bmcaddr, "BMC Address"), 
					render_edititem(username, "Username"), 
					render_edititem(password, "Password")
				       ]
				 },
			   #panel{class="edit-controls",
				  body=[
					#button{text="Cancel",
						postback=nodes_edit_cancel, delegate=?MODULE},
					#button{text="Save",
						postback={nodes_edit_save, Macs}, delegate=?MODULE}
				       ]
				 }
			  ]
		   },
    wf:insert_top("application", Panel).

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
event({node_edit, Mac}) ->
    render_edit([Mac]);
event(edit_selected) ->
    Macs = find_selected(),
    render_edit(Macs);
event(nodes_edit_cancel) ->
    wf:remove("edit-nodes");
event({nodes_edit_save, Macs}) ->
    Fields = [pool, host, type, bmcaddr, username, password],
    Values = [wf:q(Field) || Field <- Fields],
    Params = lists:zip(Fields, Values),
    [[staging_server:set_param(Mac, Param, Value) 
      || {Param, Value} <- Params, Value =/= undefined] || Mac <- Macs],

    wf:remove("edit-nodes");
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
event(Event) ->
    ok.

inplace_textbox_event(_, Val) ->
    Val.
