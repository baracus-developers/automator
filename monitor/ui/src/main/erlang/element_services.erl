-module(element_services).
-export([reflect/0, render_element/1, event/1]).

-import(nitrogen_util, [render_edititem/2]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").
-include("services.hrl").

-record(state, {record, services}).

reflect() -> record_info(fields, services).

flatten(Service) ->
    [
     Service,
     [
      #link{text="delete", delegate=?MODULE,
	    postback={service_delete, Service}}
     ]
    ].

render_table(R) ->
    {ok, Services} = services:enum(),

    Rows = [flatten(Service) || Service <- Services],

    State = #state{record=R, services=Services},

    #cbtable{class="services",
	     delegate=?MODULE,
	     postback=State,
	     selectable=true,
	     data=Rows,
	     map= [
		   name@text,
%		   elements@text,
%		   pools@text,
%		   nodes@text,
		   actions@body
		  ],
             header=[
		     #tableheader { text="Name" },
%		     #tableheader { text="Elements" },
%		     #tableheader { text="Pools" },
%		     #tableheader { text="Nodes" },
		     #tableheader { text="Actions" }
		    ],
	     rowspec=[
		      #tablecell { id=name },
%		      #tablecell { id=elements },
%		      #tablecell { id=pools },
%		      #tablecell { id=nodes },
		      #tablecell { id=actions }
		     ]
	    }.

render_body(R) ->
    [
     #h1{text="Services"},
     #panel{id="services-list", body=render_table(R)},
     #button{class="addservice-button",
	     text="+",
	     postback={service_add, R}, delegate=?MODULE}
    ].

render_element(R) ->
    Panel = #panel{body=render_body(R)},
    element_panel:render_element(Panel).

render_addservice(R) ->
    [
     render_edititem(name, "Service name"),
     #flash{},
     #panel{class="dialog-controls",
	    body=[
		  #button{text="Cancel", postback=service_canceladd,
			  delegate=?MODULE},
		  #button{text="Save", postback={service_save, R},
			  delegate=?MODULE}
		 ]
	   }
    ].

event({service_add, R}) ->
    Panel = #dialog{ id="add-service",
		     title="Add new service",
		     body=#panel{body=render_addservice(R)}
		   },
    wf:insert_top("application", Panel);

event(service_canceladd) ->
    wf:remove("add-service");
event({service_save, R}) ->
    Name = wf:q(name),
    
    case services:create(Name) of
	ok ->
	    wf:remove("add-service"),
	    wf:update("services-list", render_table(R));
	{error, Error} ->
	    Msg = wf:f("Error saving service: ~p", [Error]), 
	    wf:flash(Msg)
    end;
event({service_delete, R, Service}) ->
    ok = services:delete(Service),
    wf:update("services-list", render_table(R));
event({State, Index}) ->
    R = State#state.record,
    Module = wf:coalesce([R#services.delegate, wf_context:page_module()]),

    Service = case Index of
		  none -> none;
		  _ -> lists:nth(Index, State#state.services)
	      end,

    case R#services.postback of
	undefined -> ok;
	Postback ->
	    Module:event({Postback, Service})
    end,    
    ok;

event(_) ->
    ok.




