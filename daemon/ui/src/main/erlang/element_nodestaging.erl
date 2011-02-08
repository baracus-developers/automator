-module(element_nodestaging).
-export([reflect/0, render_element/1, event/1, inplace_textbox_event/2]).
-import(nitrogen_util, [render_edititem/2]).

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
		   sn_selected@id,
		   sn_selected@checked,
		   sn_selected@postback,
		   sn_zone@text,
		   sn_mac@text,
		   sn_pool@text,
		   sn_type@text,
		   sn_host@text,
		   sn_bmcaddr@text,
		   sn_username@text,
		   sn_password@text,
		   sn_actions@body
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
		      #tablecell { body=#checkbox{id=sn_selected, delegate=?MODULE}},
		      #tablecell { id=sn_zone },
		      #tablecell { id=sn_mac },
		      #tablecell { id=sn_pool },
		      #tablecell { id=sn_type},
		      #tablecell { id=sn_host},
		      #tablecell { id=sn_bmcaddr},
		      #tablecell { id=sn_username},
		      #tablecell { id=sn_password},
		      #tablecell { id=sn_actions }
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

render_edit(Macs) ->

    BmcTypes = [undefined | baracus_driver:get_bmctypes()],

    PoolId = wf:temp_id(),
    HostId = wf:temp_id(),
    TypeId = wf:temp_id(),
    BmcId  = wf:temp_id(),
    UserId = wf:temp_id(),
    PassId = wf:temp_id(),

    Controls = [PoolId, HostId, TypeId, BmcId, UserId, PassId],

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
					render_edititem(PoolId, "Pool"), 
					render_edititem(HostId, "Host"), 
					#panel{class="edititem",
					       body=[
						     #label{class="edititem-label", text="Type:"},
						     #dropdown{id=TypeId,
							       class="edititem-input",
							       options=[#option{text=Type, value=Type} ||
									   Type <- BmcTypes
								       ]
							      }
						    ]
					      },
					render_edititem(BmcId, "BMC Address"), 
					render_edititem(UserId, "Username"), 
					render_edititem(PassId, "Password")
				       ]
				 },
			   #panel{class="dialog-controls",
				  body=[
					#button{text="Cancel",
						postback=nodes_edit_cancel, delegate=?MODULE},
					#button{text="Save",
						postback={nodes_edit_save, Macs, Controls}, delegate=?MODULE}
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
    Value = case wf:q(Id) of
		"on" -> true;
		_ -> false
	    end,
    Db = wf:state(stagingnodes),
    F = fun(Node) ->
		Node#node{selected=Value}
	end,
    NewDb = dict:update(Mac, F, Db),
    wf:state(stagingnodes, NewDb);
event({node_deploy, Mac}) ->
    render_deploystatus([Mac]);
event({node_edit, Mac}) ->
    render_edit([Mac]);
event(edit_selected) ->
    Macs = find_selected(),
    render_edit(Macs);
event(nodes_edit_cancel) ->
    wf:remove("edit-nodes");
event({nodes_edit_save, Macs, Controls}) ->
    Fields = [pool, host, type, bmcaddr, username, password],
    Params = lists:zip(Fields, Controls),

    Values = [{Param, wf:q(Control)} || {Param, Control} <- Params],

    [[staging_server:set_param(Mac, Param, Value) 
      || {Param, Value} <- Values, Value =/= undefined] || Mac <- Macs],

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
