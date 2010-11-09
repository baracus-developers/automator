-module(element_serviceelements).
-export([reflect/0, render_element/1, event/1]).
-import(util, [render_edititem/2]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").
-include("services.hrl").

reflect() -> record_info(fields, serviceelements).

render_table(R) ->
    {ok, Elements} = services:enum_elements(R#serviceelements.name),
    
    Flatten = fun(Element) ->
		      [
		       Element#element.type,
		       "1",
		       wf:f("~B", [length(Element#element.pools)]),
		       wf:f("~B/~B", [Element#element.elasticity, 0]),
		       [
			#link{text="delete", delegate=?MODULE,
			      postback={delete_element, R, Element#element.id}}
		       ]
		      ]
	      end,

    Rows = [Flatten(Element) || Element <- Elements],

    #cbtable{class="service-elements",
	     data=Rows,
	     map= [
		   name@text,
		   zones@text,
		   pools@text,
		   nodes@text,
		   actions@body
		  ],
             header=[
		     #tableheader { text="Element" },
		     #tableheader { text="Zones" },
		     #tableheader { text="Pools" },
		     #tableheader { text="Nodes (wants/has)" },
		     #tableheader { text="Actions" }
		    ],
	     rowspec=[
		      #tablecell { id=name },
		      #tablecell { id=zones },
		      #tablecell { id=pools },
		      #tablecell { id=nodes },
		      #tablecell { id=actions }
		     ]
	    }.

render_body(R) ->
    [
     #h1{text=wf:f("~s", [R#serviceelements.name])},
     #h3{text="Elements:"},
     #panel{id="element-table", body=render_table(R)},
     #button{class="addelement-button",
	     text="+",
	     postback={element_add, R}, delegate=?MODULE},
     #h3{text="Controls:"}
    ].

render_element(R) ->
    Panel = #panel{id=R#serviceelements.id,
		   anchor=R#serviceelements.anchor,
		   class=R#serviceelements.class,
		   body=render_body(R)
		  },
    element_panel:render_element(Panel).

render_addelement(R) ->
    Types = ["KVM Hypervisor", "CEPH Storage", "NFS Storage"],

    [
     #panel{class="edititem",
	    body=[
		  #label{class="edititem-label", text="Type:"},
		  #dropdown{id="type",
			    class="edititem-input",
			    options=[#option{text=Type, value=Type} ||
					Type <- Types
				    ]
			   }
		 ]
	   },
     #panel{class="edititem",
	    body=[
		  #label{class="edititem-label", text="Pool Associations:"},
		  #poollist{hid="selected-pools"}
		 ]
	   },
     render_edititem("elasticity", "Elasticity"),
     #flash{},
     #panel{class="dialog-controls",
	    body=[
		  #button{text="Cancel", postback=element_canceladd,
			  delegate=?MODULE},
		  #button{text="Save", postback={element_save, R},
			  delegate=?MODULE}
		 ]
	   }
    ].

event({element_add, R}) ->
    Panel = #dialog{ id="add-element",
		     title="Add new service element",
		     body=#panel{body=render_addelement(R)}
		   },
    wf:insert_top("application", Panel);

event(element_canceladd) ->
    wf:remove("add-element");
event({element_save, R}) ->
    Type = wf:q("type"),
    Pools = element_poollist:get_selected("selected-pools"),
    {Elasticity, _} = string:to_integer(wf:q("elasticity")),

    case services:create_element(R#serviceelements.name,
				 Type, Pools, Elasticity) of
	ok ->
	    wf:remove("add-element"),
	    wf:update("element-table", render_table(R));
	{error, Error} ->
	    Msg = wf:f("Error saving element: ~p", [Error]), 
	    wf:flash(Msg)
    end;

event({delete_element, R, Id}) ->
    ok = services:delete_element(Id),
    wf:update("element-table", render_table(R));   

event(_) ->
     ok.
